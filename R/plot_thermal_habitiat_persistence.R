#' plot thermal habitat persistence
#'
#' plots thermal_habitat_persistence data set.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'

#'

plot_thermal_habitat_persistence <- function(shadedRegion = NULL,
                                      report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB","GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  #fix <- ecodata::thermal_habitat_persistence |>

  neus_map  <-  ggplot2::map_data('worldHires',region = 'USA')


  p <-
    ggplot2::ggplot()+
    ggplot2::geom_tile(data = d, ggplot2::aes(x=longitude,y = latitude,color = Ndays),linewidth = 2)+
    ggplot2::facet_wrap(~temp.threshold,labeller = 'label_both')+
    ggplot2::annotation_map(neus_map,fill = 'grey70')+
    ggplot2::scale_color_viridis_c()+
    ggplot2::xlab('')+
    ggplot2::ylab('')+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = 'bottom',
          panel.grid = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5))


    return(p)

}
