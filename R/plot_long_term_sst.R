#' plot long term sea surface temperature
#'
#' plots long_term_sst data set. This is a shelf wide product
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_long_term_sst <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              n = 0) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
   fix<- ecodata::long_term_sst |>
     dplyr::mutate(hline = mean(Value, na.rm = TRUE))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::ggtitle("Long-term SST")+
    ggplot2::ylab(expression("Temperature (\u00B0C)"))+
    ggplot2::xlab(ggplot2::element_blank())+
    ecodata::geom_gls()+
    ecodata::geom_lm(n=n) +
    ecodata::theme_ts()+
    ecodata::theme_title()



    return(p)

}

attr(plot_long_term_sst,"report") <- c("MidAtlantic","NewEngland")
