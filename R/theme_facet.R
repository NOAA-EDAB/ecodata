#'A ggplot2 theme for facetted SOE time series figures
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
  theme(
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    legend.key = element_blank(),
    axis.title = element_text(size = 10)
  )
}
