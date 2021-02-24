#'A ggplot2 ggtitle formmatting for SOE figures
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
#'   theme_title()
#'
#'
theme_title <- function(...){
  theme(
    plot.title = element_text(size = 10)
  )
}
