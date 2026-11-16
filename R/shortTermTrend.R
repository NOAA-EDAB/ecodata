#'A geom to fit a LM model to a short time series
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
#'@param n Numeric. Number of points to use for trend. Default = 10.
#'@param nBootSamples Numeric. Number of bootstrap samples used to test Null hypothesis. Default = 499
#'@param pValThreshold Numeric. Significance level of the test. Default = 0.05
#'@param ... Other arguments may be passed to the stat, including fixed aesthetics.
#'
#'
#'@export
#'
#'@return Outputs a trendline (solid if p < pValThreshold, dashed if pValThreshold <= p < 0.20).
#'
#'
#'@examples
#'library(ggplot2)
#'
#'#Generate series
#'
#'m <- 0.1
#'x <- 1:10
#'y <-  m*x + rnorm(10, sd = 0.35)
#'
#'data <- data.frame(x = x,
#'                   y = y)
#'
#'#Plot series with trend
#'ggplot(data = data) +
#'   geom_line(aes(x = x, y = y)) +
#'   geom_lm(aes(x = x, y = y), n = 10)

geom_lm <- function(
  mapping = NULL,
  data = NULL,
  stat = "LM",
  position = "identity",
  show.legend = NA,
  na.rm = FALSE,
  inherit.aes = TRUE,
  warn = TRUE,
  n = 10,
  nBootSamples = 499,
  pValThreshold = 0.05,
  ...
) {
  ggplot2::layer(
    geom = GeomLM,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      na.rm = FALSE,
      nBootSamples = nBootSamples,
      pValThreshold = pValThreshold,
      ...
    )
  )
}

## GeomLM based on GeomGLS - add dashed linetype logic and visibility gate
GeomLM <- ggplot2::ggproto(
  "GeomLM",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  extra_params = c("n", "na.rm", "pValThreshold", "nBootSamples"),

  default_aes = ggplot2::aes(
    size = 2,
    color = NA,
    fill = NA,
    linetype = 1,
    alpha = 0.7
  ),

  draw_key = ggplot2::draw_key_path,

  draw_group = function(data, panel_params, coord) {
    coords <- coord$transform(data, panel_params)
    first_row <- coords[1, , drop = FALSE]

    # The Visibility Gatekeeper: If p >= 0.20, draw nothing
    if (!is.null(first_row$is_plotted) && !isTRUE(first_row$is_plotted)) {
      return(grid::nullGrob())
    }

    # Select default color based on positive/negative trend
    if (coords$y[1] < coords$y[which.max(coords$x)]) {
      first_row$color <- "orange4"
    } else {
      first_row$color <- "purple4"
    }

    # Contextual Linetype Logic: Solid (1) if p < 0.05, Dashed (2) if 0.05 <= p < 0.20
    if (!is.null(first_row$is_sig)) {
      first_row$linetype <- ifelse(isTRUE(first_row$is_sig), 1, 2)
    }

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

#' StatLM Stat proto based on ecodata StatGLS
#' Need to pass arguments
#'
#' nBootSamples, pVal significance, number datapoints
#'

StatLM <- ggplot2::ggproto(
  "StatLM",
  ggplot2::Stat,
  required_aes = c("x", "y"),

  extra_params = c("n", "pValThreshold", "nBootSamples", "na.rm"),
  compute_group = function(data, scales, warn, n, pValThreshold, nBootSamples) {
    # Helper to pass NA statistics when the trend cannot be evaluated
    pass_na_stats <- function() {
      data.frame(
        x = if (nrow(data) > 0) data$x[1] else NA,
        y = if (nrow(data) > 0) data$y[1] else NA,
        is_sig = FALSE,
        is_plotted = FALSE,
        pval = NA,
        pval_chi2 = NA,
        slope = NA
      )
    }

    nlimit <- 7
    if (n <= nlimit) {
      return(pass_na_stats())
    }

    if (n > nrow(data)) {
      n <- nrow(data)
    }

    dataUse <- data |>
      dplyr::arrange(x) |>
      dplyr::filter(x %in% (max(x) - (n - 1)):max(x)) |>
      dplyr::mutate(x = x - min(x) + 1)

    if (nrow(dataUse) < nlimit) {
      return(pass_na_stats())
    }

    xmax <- max(data$x)
    xmin <- xmax - n + 1

    set.seed(1234)
    linear_ar1 <- try(
      arfit::fit_real_data(dataUse, nBootSims = nBootSamples),
      silent = TRUE
    )

    if (inherits(linear_ar1, "try-error") || is.na(linear_ar1$pValue)) {
      return(pass_na_stats())
    }

    # ALWAYS calculate coordinates to pass the stats through ggplot_build
    dataNAs <- linear_ar1$data
    coefs <- linear_ar1$alt$betaEst
    xMat <- as.matrix(cbind(rep(1, n), dataNAs$x))
    predy <- xMat %*% coefs
    newtime <- seq(xmin, xmax, length.out = n)

    # Significance and plotting gates
    boot_sig <- linear_ar1$pValue < pValThreshold
    chi2_sig <- !is.na(linear_ar1$pValChi2) &&
      linear_ar1$pValChi2 < pValThreshold

    fittedData <- data.frame(
      x = newtime,
      y = predy,
      is_sig = boot_sig, # Solid if bootstrap is significant
      is_plotted = boot_sig || chi2_sig, # Plot if EITHER bootstrap or chi2 is significant
      pval = linear_ar1$pValue,
      pval_chi2 = linear_ar1$pValChi2,
      slope = linear_ar1$alt$betaEst[2]
    )

    return(fittedData)
  }
)
