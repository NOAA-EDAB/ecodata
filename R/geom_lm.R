#'A geom to fit a LM model to a short time series
#'
#'@param mapping Set of aesthetic mappings created by \code{aes()}. By default \code{inherit.aes = TRUE}, which
#'assigns the top-level plotting \code{aes()} to the GLS geom.
#'
#'@param data Input series to be analyzed. If NULL, data is inherited from previous layer or \code{ggplot} call.
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
#'x <- 1:10
#'y <-  m*x + rnorm(10, sd = 0.35)
#'
#'data <- data.frame(x = x,
#'                   y = y)
#'
#'#Plot series with trend
#'ggplot(data = data) +
#'   geom_line(aes(x = x, y = y)) +
#'   geom_lm(aes(x = x, y = y))

geom_lm <- function(mapping = NULL, data = NULL, stat = "LM",
                     position = "identity",show.legend = NA, na.rm = FALSE,
                     inherit.aes = TRUE, warn = TRUE, n= 10, nBootSamples = 499,
                     pValThreshold = 0.05, ...) {
  ggplot2::layer(
    geom = arfit:::GeomLM, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = FALSE,nBootSamples = nBootSamples,
                  pValThreshold = pValThreshold, ...)

  )
}
