#'A geom to fit a GLS model to a time series
#'
#'@param mapping Set of aesthetic mappings created by \code{aes()}. By default \code{inherit.aes = TRUE}, which
#'assigns the top-level plotting \code{aes()} to the GLS geom.
#'
#'@param data Input series to be analyzed. If NULL, data is inherited from previous layer or \code{ggplot} call.
#'
#'@param ... Other arguments may be passed to the stat, including fixed aesthetics (e.g. color = "orange").
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
#'   stat_gls(aes(x = x, y = y))

geom_gls <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA, 
                    inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = ecodata:::fitGLS, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
