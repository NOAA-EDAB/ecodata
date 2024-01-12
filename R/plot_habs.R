#' plot harmful algal bloom
#'
#' Plots Alexandrium counts or PSP
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Variable to plot ("Alexandrium", "PSP")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_habs <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              varName = "Alexandrium") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM","NE")
  }

  if (varName == "PSP"){
    plotTitle <- "Percent PSP Samples in Blue Mussels"
    plotylab <- "Percentage of samples that exceed the threshold"
    plotLegend <- "State"
  } else if(varName == "Alexandrium") {
    plotTitle <- "Gulf of Maine Alexandrium Cyst Abundance"
    plotylab <- expression("Cyst Abundance (10"^6*"Cells)")
    plotLegend <- "Region"

  }

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max

  p <-  ecodata::habs |>
     dplyr::filter(Source == varName,
                   EPU %in% filterEPUs) |>
     ggplot2::ggplot(ggplot2::aes(x = Time, y = Value, color = Var))+
     ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                       xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                       ymin = -Inf, ymax = Inf) +
     ggplot2::geom_point()+
     ggplot2::geom_line()+
     ecodata::theme_ts()+
     ggplot2::scale_color_discrete(name = plotLegend)+
     ecodata::theme_title()+
     ggplot2::ylab(plotylab)+
     ggplot2::xlab(ggplot2::element_blank())+
     ggplot2::ggtitle(plotTitle)+
     ecodata::geom_gls()+
     ecodata::theme_facet()


    return(p)


}
attr(plot_habs,"varName") <- c("Alexandrium","PSP")
attr(plot_habs,"report") <- c("MidAtlantic","NewEngland")

