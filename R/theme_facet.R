#'A ggplot2 theme for faceted SOE time series figures
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' data <- data.frame(x = rep(1:10,2),
#'                    y = rnorm(20),
#'                    Var = rep(c("group 1","group 2"), each = 10))
#'
#' #Plot series with trend and SOE plot theme
#' ggplot(data = data) +
#'   geom_line(aes(x = x, y = y)) +
#'   facet_wrap(Var~.)+
#'   theme_facet()
#'
#'
theme_facet <- function(...){
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.75),
    legend.key = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(size = 10)
  )
}
