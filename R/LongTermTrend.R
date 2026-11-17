#'A geom to fit a GLS model to a time series
#'
#'@param mapping Set of aesthetic mappings created by \code{aes()}. By default \code{inherit.aes = TRUE}, which
#'assigns the top-level plotting \code{aes()} to the GLS geom.
#'@param data Input series to be analyzed. If NULL, data is inherited from previous layer or \code{ggplot} call.
#'@param stat stat
#'@param position position
#'@param na.rm remove NAs
#'@param show.legend show legend
#'@param inherit.aes inherit aesthetics
#'@param warn Conditional. If \code{TRUE}, a warning message will be returned when N < 30.
#'
#'@param ... Other arguments may be passed to the stat, including fixed aesthetics.
#'
#'
#'@export
#'
#'@return Outputs a normal linear trendline (solid if p < 0.05, dashed if 0.05 <= p < 0.20).
#'
#'@examples
#'library(ggplot2)
#'
#'#Generate series
#'
#'m <- 0.1
#'x <- 1:30
#'y <-  m*x + rnorm(30, sd = 0.35)
#'
#'data <- data.frame(x = x,
#'                   y = y)
#'
#'#Plot series with trend
#'ggplot(data = data) +
#'   geom_line(aes(x = x, y = y)) +
#'   geom_gls(aes(x = x, y = y))

geom_gls <- function(
  mapping = NULL,
  data = NULL,
  stat = "GLS",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  warn = TRUE,
  ...
) {
  ggplot2::layer(
    geom = GeomGLS,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, warn = warn, ...)
  )
}

# defines line appearance
GeomGLS <- ggplot2::ggproto(
  "GeomGLS",
  ggplot2::Geom,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(
    size = 2,
    color = NA,
    fill = NA,
    linetype = 1,
    alpha = 0.5
  ),

  draw_key = ggplot2::draw_key_path,

  draw_group = function(data, panel_params, coord) {
    coords <- coord$transform(data, panel_params)
    first_row <- coords[1, , drop = FALSE]

    # The Visibility Gatekeeper: If p >= 0.05, draw nothing
    if (!is.null(first_row$is_plotted) && !isTRUE(first_row$is_plotted)) {
      return(grid::nullGrob())
    }

    # Select default color based on positive/negative trend
    if (coords$y[1] < coords$y[which.max(coords$x)]) {
      first_row$color <- "orange"
    } else {
      first_row$color <- "purple"
    }

    # Contextual Linetype Logic: Dashed (2) if autocorrelated, Solid (1) if not
    if (!is.null(first_row$is_autocorrelated)) {
      first_row$linetype <- ifelse(isTRUE(first_row$is_autocorrelated), 2, 1)
    }

    # Return just the trendline
    grid::linesGrob(
      coords$x,
      coords$y,
      gp = grid::gpar(
        col = first_row$color,
        alpha = first_row$alpha,
        lwd = first_row$size * ggplot2::.pt,
        lty = first_row$linetype
      )
    )
  }
)

# where all the computation takes place
StatGLS <- ggplot2::ggproto(
  "StatGLS",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales, warn) {
    `%>%` <- magrittr::`%>%`
    data <- data %>%
      dplyr::arrange(x) %>%
      tidyr::complete(x = tidyr::full_seq(min(data$x):max(data$x), 1))

    # Minimum Sample Size Gate: Do not evaluate if fewer than 10 valid years
    if (sum(!is.na(data$y)) < 10) {
      return(data.frame(x = numeric(0), y = numeric(0)))
    }

    if (warn & nrow(data) < 30) {
      message("N < 30")
    }

    # Model fitting (suppressing errors) -----------------------------------
    constant_norm <- try(
      nlme::gls(y ~ 1, data = data, na.action = na.omit),
      silent = TRUE
    )
    constant_ar1 <- try(
      nlme::gls(
        y ~ 1,
        data = data,
        correlation = nlme::corAR1(form = ~x),
        na.action = na.omit
      ),
      silent = TRUE
    )
    linear_norm <- try(
      nlme::gls(y ~ x, data = data, na.action = na.omit),
      silent = TRUE
    )
    linear_ar1 <- try(
      nlme::gls(
        y ~ x,
        data = data,
        correlation = nlme::corAR1(form = ~x),
        na.action = na.omit
      ),
      silent = TRUE
    )

    # Initialize stats as NA
    norm_aicc <- NA
    norm_pval <- NA
    norm_slope <- NA
    ar1_aicc <- NA
    ar1_pval <- NA
    ar1_slope <- NA

    # Extract stats if normal models succeeded
    if (
      !inherits(constant_norm, "try-error") &&
        !inherits(linear_norm, "try-error")
    ) {
      norm_aicc <- AICcmodavg::AICc(linear_norm)
      norm_pval <- anova(
        update(constant_norm, method = "ML"),
        update(linear_norm, method = "ML")
      )$`p-value`[2]
      norm_slope <- as.numeric(coef(linear_norm)["x"])
    }

    # Extract stats if AR1 models succeeded
    if (
      !inherits(constant_ar1, "try-error") && !inherits(linear_ar1, "try-error")
    ) {
      ar1_aicc <- AICcmodavg::AICc(linear_ar1)
      ar1_pval <- anova(
        update(constant_ar1, method = "ML"),
        update(linear_ar1, method = "ML")
      )$`p-value`[2]
      ar1_slope <- as.numeric(coef(linear_ar1)["x"])
    }

    # Abort if the Normal linear model failed (since it's the geometric baseline)
    if (is.na(norm_aicc)) {
      return(data.frame(x = numeric(0), y = numeric(0)))
    }

    # Determine if system is autocorrelated (AR1 provides a better fit)
    sys_autocorrelated <- FALSE
    if (!is.na(ar1_aicc) && ar1_aicc < norm_aicc) {
      sys_autocorrelated <- TRUE
    }

    # Evaluate strict significance for the plot gate
    is_significant <- !is.na(norm_pval) && norm_pval < 0.05

    # ALWAYS calculate the normal model coordinates to pass the stats through ggplot_build
    newtime <- seq(min(data$x), max(data$x), length.out = length(data$x))
    newdata <- data.frame(x = newtime, x2 = newtime^2)
    lm_pred <- AICcmodavg::predictSE(
      linear_norm,
      newdata = newdata,
      se.fit = TRUE
    )

    return(data.frame(
      x = newtime,
      y = lm_pred$fit,
      is_sig = is_significant,
      is_plotted = is_significant, # <--- Flag tells Geom strictly to draw if p < 0.05
      is_autocorrelated = sys_autocorrelated,
      norm_aicc = norm_aicc,
      norm_pval = norm_pval,
      norm_slope = norm_slope,
      ar1_aicc = ar1_aicc,
      ar1_pval = ar1_pval,
      ar1_slope = ar1_slope
    ))
  }
)
