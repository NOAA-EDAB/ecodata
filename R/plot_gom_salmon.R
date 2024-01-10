#' plots Gulf of Maine Salmon
#'
#' plots gom_salmon data set
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_gom_salmon <- function(shadedRegion = NULL,
                            report="NewEngland") {

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
  fix<- ecodata::gom_salmon

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  yaxisLabel <- fix |>
    dplyr::distinct(Var,Units)


  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::facet_wrap(~as.factor(Var),
                        scales="free_y",
                        strip.position = "left",
                        labeller = ggplot2::as_labeller(c(Total = yaxisLabel$Units[yaxisLabel$Var=="Total"], PSAR = yaxisLabel$Units[yaxisLabel$Var=="PSAR"]) ) )+
    ggplot2::ggtitle("Atlantic Salmon")+
    #ggplot2::ylab()+
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ylab(ggplot2::element_blank())+
    ecodata::geom_gls()+
    ecodata::theme_ts()+
    ecodata::theme_title()+
    ggplot2::theme(strip.placement = "outside",
                   strip.background = ggplot2::element_blank())


    return(p)

}
