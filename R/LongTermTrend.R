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
#'@return Outputs a trendline (solid for normal errors, dashed for AR1 errors) and appends an asterisk if p < 0.05.
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
    last_row <- coords[nrow(coords), , drop = FALSE]

    # Select default color based on positive/negative trend
    if (coords$y[1] < coords$y[which.max(coords$x)]) {
      first_row$color <- "orange"
    } else {
      first_row$color <- "purple"
    }

    # Override linetype based on the winning model's error structure
    if (!is.null(first_row$model_type)) {
      first_row$linetype <- ifelse(first_row$model_type == "linear_ar1", 2, 1) # 2 = dashed, 1 = solid
    }

    # Create the trendline
    line_grob <- grid::linesGrob(
      coords$x,
      coords$y,
      default.units = "npc", # Explicitly set default units for the line
      gp = grid::gpar(
        col = first_row$color,
        alpha = first_row$alpha,
        lwd = first_row$size * ggplot2::.pt,
        lty = first_row$linetype
      )
    )

    # Conditionally create the asterisk for significant trends
    if (!is.null(first_row$is_sig) && isTRUE(first_row$is_sig)) {
      star_grob <- grid::textGrob(
        label = "*",
        # FIX: Wrap raw coordinates in grid::unit(..., "npc") before arithmetic
        x = grid::unit(last_row$x, "npc") + grid::unit(2, "mm"),
        y = grid::unit(last_row$y, "npc"),
        gp = grid::gpar(col = first_row$color, fontsize = 16)
      )
      # Return both graphics
      return(grid::gList(line_grob, star_grob))
    }

    # Return only the trendline if not significant
    return(line_grob)
  }
)

# where all the computation takes place
# where all the computation takes place
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
    ar1_aicc <- NA
    ar1_pval <- NA

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
    }

    # Abort if both linear models failed
    if (is.na(norm_aicc) && is.na(ar1_aicc)) {
      return(data.frame(x = numeric(0), y = numeric(0)))
    }

    # Determine the winning model (handling NAs safely)
    min_aicc <- min(c(norm_aicc, ar1_aicc), na.rm = TRUE)

    if (!is.na(norm_aicc) && norm_aicc == min_aicc) {
      best_model_name <- "linear_norm"
      model <- linear_norm
      best_pval <- norm_pval
    } else {
      best_model_name <- "linear_ar1"
      model <- linear_ar1
      best_pval <- ar1_pval
    }

    # Always generate the BLUE line
    newtime <- seq(min(data$x), max(data$x), length.out = length(data$x))
    newdata <- data.frame(x = newtime, x2 = newtime^2)
    lm_pred <- AICcmodavg::predictSE(model, newdata = newdata, se.fit = TRUE)

    # Return coordinates alongside wide-format model stats
    data.frame(
      x = newtime,
      y = lm_pred$fit,
      model_type = best_model_name,
      is_sig = !is.na(best_pval) && best_pval < 0.05,
      norm_aicc = norm_aicc,
      norm_pval = norm_pval,
      ar1_aicc = ar1_aicc,
      ar1_pval = ar1_pval
    )
  }
)

get_gls_stats <- function(p) {
  built_p <- ggplot2::ggplot_build(p)
  gls_data <- built_p$data[[4]]

  if ("norm_aicc" %in% names(gls_data)) {
    # Grab the single unique row of stats
    raw_stats <- unique(gls_data[, c(
      "model_type",
      "norm_aicc",
      "norm_pval",
      "ar1_aicc",
      "ar1_pval"
    )])

    # Restructure into a clean, readable dataframe
    fit_stats <- data.frame(
      model = c("linear_norm", "linear_ar1"),
      aicc = c(raw_stats$norm_aicc, raw_stats$ar1_aicc),
      pval = c(raw_stats$norm_pval, raw_stats$ar1_pval),
      is_winning_model = c(
        raw_stats$model_type == "linear_norm",
        raw_stats$model_type == "linear_ar1"
      )
    )
  } else {
    fit_stats <- NULL
  }

  return(fit_stats)
}
