#'A geom to fit a regime shifts to a time series
#'
#'@param data Input series to be analyzed. If NULL,
#'data is inherited from previous layer or \code{ggplot} call.
#'
#'@param ... Other arguments may be passed to the stat, including fixed aesthetics.
#'
#'@export
#'
#'@return If regime shifts exists, then xintercept line(s) is returned. Otherwise output is \code{NULL}.
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
#'   geom_regime()

geom_regime <- function(data = NULL, mapping = NULL,
                       stat = "REGIME",
                       geom = "vline",
                       position = "identity",
                       inherit.aes = TRUE,
                       na.rm = FALSE,
                       color = "red",
                       show.legend = NA) {


  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      color = color
    )
  )
}
