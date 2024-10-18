#'A ggplot2 theme for SOE time series figures
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' m <- 0.1
#' x <- 1:30
#' y <-  m*x + rnorm(30, sd = 0.35)
#'
#' data <- data.frame(x = x,
#'                   y = y)
#'
#' #Plot series with trend and SOE plot theme
#' ggplot(data = data) +
#'   geom_line(aes(x = x, y = y)) +
#'   stat_gls(aes(x = x, y = y, color = stat(col)))+
#'   theme_ts()

theme_ts <- function(...){
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.75),
    legend.key = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(size = 10)
  )
}
