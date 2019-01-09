#'A ggplot2 theme for SOE time series figures
#'
#' @export
#'
#' @examples
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
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    legend.key = element_blank()
  )
}
