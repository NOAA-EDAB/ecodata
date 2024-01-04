#' plot stomach fullness
#'
#' plots [stom_fullness]
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_stom_fullness <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              EPU = "MAB") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- "MAB"
  } else {
    filterEPUs <- EPU
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
   fix<- ecodata::stom_fullness |>
     dplyr::filter(EPU %in% filterEPUs)

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
    ggplot2::geom_line()+
    ggplot2::ggtitle(paste0(EPU,": Stomach fullness"))+
    ggplot2::ylab("Stomach fullness anomaly") +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::facet_wrap(.~Var)+
    #ecodata::geom_gls()+
    #ecodata::theme_ts()+
    #ecodata::theme_facet()+
    ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
                   legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45))+
    ecodata::theme_title()

   # # optional code for New England specific (2 panel) formatting
   #  if (report == "NewEngland") {
   #    p <- p +
   #      ggplot2::theme(legend.position = "bottom",
   #                     legend.title = ggplot2::element_blank())
   #
   #  }

    return(p)


  #
  #

}
