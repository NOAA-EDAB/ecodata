#' plot thermal habitat persistence
#'
#' plots thermal_habitat_gridded data set. Current SOE Year only
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#' @export

#'

plot_thermal_habitat_gridded <- function(shadedRegion = NULL,
                                             report="MidAtlantic",
                                             year = NULL,
                                          thresholds = c(15,24),
                                        depths = 'AllDepths') {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB","GOM")
  }

  if (is.null(year)) {
    # current SOE report year
    Yr <- max(ecodata::thermal_habitat_gridded$Time)
  } else {
    Yr <- year
  }
  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  fix <- ecodata::thermal_habitat_gridded |>
    dplyr::filter(Var %in% thresholds,
                  Depth %in% depths)|>
    dplyr::mutate(Var = paste0(Var,"\u00B0C")) |>
    dplyr::filter(Time == Yr)

  # neus_map  <- ggplot2::map_data('worldHires',region = 'USA')
  legendTitle <- unique(fix$Units)

  p <- fix |>
    ggplot2::ggplot()+
    ggplot2::geom_tile(ggplot2::aes(x=Longitude,y = Latitude, color = Value, width = 1/12, height = 1/12),
                       linewidth = setup$line.size) +
    ggplot2::geom_sf(data=ecodata::coast, size = setup$map.lwd) +
    ggplot2::facet_grid(Depth~Var)+
    ggplot2::scale_color_viridis_c(legendTitle)+
    ggplot2::coord_sf(xlim = c(setup$xmin,setup$xmax), ylim = c(setup$ymin,setup$ymax)) +
    #ggplot2::annotation_map(neus.map,fill = "grey70")+

    ggplot2::xlab('')+
    ggplot2::ylab('')+
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title() +
    ggplot2::ggtitle(paste(Yr)) +
    ggplot2::theme(legend.position = "bottom")



    return(p)

}
attr(plot_thermal_habitat_gridded,"report") <- c("MidAtlantic","NewEngland")
attr(plot_thermal_habitat_gridded,"year") <- NULL
