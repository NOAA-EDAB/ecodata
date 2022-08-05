#'A geom to fit a GLS model to a time series
#'
#'@param mapping Set of aesthetic mappings created by \code{aes()}. By default \code{inherit.aes = TRUE}, which
#'assigns the top-level plotting \code{aes()} to the GLS geom.
#'
#'@param data Input series to be analyzed. If NULL, data is inherited from previous layer or \code{ggplot} call.
#'
#'@param warn Conditional. If \code{TRUE}, a warning message will be returned when N < 30.
#'
#'@param ... Other arguments may be passed to the stat, including fixed aesthetics.
#'
#'
#'@export
#'
#'@return If slope is significantly different from 0 (p < 0.05), then a line of best fit is returned. Otherwise output
#'is \code{NULL}.
#'
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

geom_regime <- function(mapping = NULL, data = NULL,
                       ...,
                       xintercept = NULL,
                       na.rm = FALSE,
                       show.legend = NA) {

  # Act like an annotation
  if (!missing(xintercept)) {
    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      cli::cli_warn("{.fn geom_vline}: Ignoring {.arg mapping} because {.arg xintercept} was provided.")
    }
    if (!is.null(data)) {
      cli::cli_warn("{.fn geom_vline}: Ignoring {.arg data} because {.arg xintercept} was provided.")
    }

    data <- data_frame(xintercept = xintercept)
    mapping <- ggplot2::aes(xintercept = xintercept)
    show.legend <- FALSE
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomREGIME,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
