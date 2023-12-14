#' plot heatwave_peak_date
#'
#' Map of SST anomaly on date of peak extreme temperature event for a given year.
#' NOTE DATE NOT CONTAINED IN DATASET. SHOULD BE
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_heatwave_peak_date <- function(shadedRegion = shadedRegion,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
    # Set lat/lon window for maps
    setup$xmin = -81
    setup$xmax = -66
    setup$ymin = 35.5
    setup$ymax = 43
    setup$xlims <- c(setup$xmin, setup$xmax)
    setup$ylims <- c(setup$ymin, setup$ymax)

  } else {
    filterEPUs <- c("GB", "GOM")
    # Set lat/lon window for maps
    setup$xmin = -73
    setup$xmax = -65
    setup$ymin = 39
    setup$ymax = 45
    setup$xlims <- c(setup$xmin, setup$xmax)
    setup$ylims <- c(setup$ymin, setup$ymax)

  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  #EPU shapefile
  report_epu_sf <- ecodata::epu_sf |>
    sf::st_as_sf() |>
    dplyr::filter(EPU %in% filterEPUs)

  hw <- ecodata::heatwave_peak_date |>
    dplyr::filter(EPU %in% filterEPUs) |>
    dplyr::mutate(Value = replace(Value, Value > 4, 4))

  # included DATE in dataset for adding to plot title


  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <-
    ggplot2::ggplot() +
    ggplot2::geom_tile(data =hw, ggplot2::aes(x = Longitude, y = Latitude,fill = Value)) +
    ggplot2::geom_sf(data = ecodata::coast, size = setup$map.lwd) +
    ggplot2::geom_sf(data = report_epu_sf, fill = "transparent", size = setup$map.lwd) +
    ggplot2::scale_fill_gradientn(name = "Temp.\nAnomaly (C)",
                                  colours = c(scales::muted("blue"), "white",
                                              scales::muted("red"), "black"),
                                  values = scales::rescale(c(-4,0,4,8)),
                                  guide = "colorbar", limits=c(-4,8)) +
    ggplot2::coord_sf(crs = setup$crs, xlim = setup$xlims, ylim = setup$ylims) +
    ggplot2::facet_wrap(EPU~.) +
    ecodata::theme_map() +
    ggplot2::ggtitle(paste(setup$region, "heatwave anomaly")) + #add date
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.75),
                   legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = 11),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(hjust=0),
                   axis.text = ggplot2::element_text(size = 8))+
    ecodata::theme_title()

   # optional code for New England specific (2 panel) formatting
    if (report == "NewEngland") {
      p <- p +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())

    }

    return(p)

  # Paste commented original plot code chunk for reference
  # LTL_MAB.Rmd-heatwave-anom-grid
  # #EPU shapefile
  # mab_epu_sf <- ecodata::epu_sf %>%
  #   dplyr::filter(EPU %in% c("MAB"))
  #
  # #Map line parameters
  # map.lwd <- 0.4
  #
  # # Set lat/lon window for maps
  # xmin = -81
  # xmax = -66
  # ymin = 35.5
  # ymax = 43
  # xlims <- c(xmin, xmax)
  # ylims <- c(ymin, ymax)
  # hw <- ecodata::heatwave_peak_date %>%
  #   dplyr::filter(EPU == "MAB") %>%
  #   dplyr::mutate(Value = replace(Value, Value > 4, 4))
  #
  # mab_map <-
  #   ggplot2::ggplot() +
  #   ggplot2::geom_tile(data =hw, aes(x = Longitude, y = Latitude,fill = Value)) +
  #   ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  #   ggplot2::geom_sf(data = mab_epu_sf, fill = "transparent", size = map.lwd) +
  #   ggplot2::scale_fill_gradientn(name = "Temp.\nAnomaly (C)",
  #                                 colours = c(scales::muted("blue"), "white",
  #                                             scales::muted("red"), "black"),
  #                                 values = scales::rescale(c(-4,0,4,8)),
  #                                 guide = "colorbar", limits=c(-4,8)) +
  #
  #   # ggplot2::scale_fill_gradient2(name = "Temp Anomaly (C)",
  #   #                      low = scales::muted("blue"),
  #   #                      mid = "white",
  #   #                      midpoint = 0,
  #   #                      high = scales::muted("red"),
  #   #                      guide = "colourbar",
  #   #                      limits = c(-4,8)) +
  #   ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  #   #facet_wrap(Season~.) +
  #   ecodata::theme_map() +
  #   ggplot2::ggtitle("MAB heatwave anomaly (July 28, 2020)") +
  #   ggplot2::xlab("Longitude") +
  #   ggplot2::ylab("Latitude") +
  #   ggplot2::theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
  #                  legend.key = element_blank(),
  #                  axis.title = element_text(size = 11),
  #                  strip.background = element_blank(),
  #                  strip.text=element_text(hjust=0),
  #                  axis.text = element_text(size = 8))+
  #   ecodata::theme_title()
  #
  #
  # mab_map
  #
  #

}
