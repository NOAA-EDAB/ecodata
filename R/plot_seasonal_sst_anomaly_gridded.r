#' plot seasonal_sst_anomaly_gridded
#'
#' @param season Character string. Season to plot. (Default = NULL, plot all seasons)
#' @param region Character vector. Regional EPUs ("GB","MAB") to overly on figure. (Default = NULL, use all)
#'
#'
#' @return ggplot object
#'
#' @export

plot_seasonal_sst_anomaly_gridded <- function(shadedRegion = NULL,
                                              report = "MidAtlantic") {


  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")

    xmin = -77
    xmax = -66
    ymin = 35.5
    ymax = 43
    xlims <- c(xmin, xmax)
    ylims <- c(ymin, ymax)


  } else {
    filterEPUs <- c("GB", "GOM")
    # Set lat/lon window for maps
    xmin = -73
    xmax = -65
    ymin = 39
    ymax = 45
    xlims <- c(xmin, xmax)
    ylims <- c(ymin, ymax)
  }


  #EPU shapefile
  ne_epu_sf <- ecodata::epu_sf  |>
    sf::st_as_sf() |>
    dplyr::filter(EPU %in% filterEPUs)

  sst <- ecodata::seasonal_sst_anomaly_gridded |>
    dplyr::mutate(Season = factor(Season, levels = c("Winter",
                                                     "Spring",
                                                     "Summer",
                                                     "Fall"))) |>
    dplyr::mutate(Value = replace(Value, Value > 5, 5))


  # filter by season
  # sst <- sst %>%
  #   dplyr::filter(Season %in% season)

  p <-
    ggplot2::ggplot() +
    ggplot2::geom_tile(data = sst, ggplot2::aes(x = Longitude, y = Latitude,fill = Value)) +
    ggplot2::geom_sf(data = ecodata::coast, size = setup$map.lwd) +
    ggplot2::geom_sf(data = ne_epu_sf, fill = "transparent", size = setup$map.lwd) +
    ggplot2::scale_fill_gradient2(name = "Temp.\nAnomaly (C)",
                                  low = scales::muted("blue"),
                                  mid = "white",
                                  high = scales::muted("red"),
                                  limits = c(-5,5),
                                  labels = c("<-5", "-2", "0", "2", ">5")) +
    ggplot2::coord_sf(xlim = xlims, ylim = ylims) +
    ggplot2::facet_wrap(Season~.) +
    ecodata::theme_map() +
    ggplot2::ggtitle("SST anomaly (2022)") +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.75),
                   legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = 11),
                   strip.background = ggplot2::element_blank(),
                   strip.text=ggplot2::element_text(hjust=0),
                   axis.text = ggplot2::element_text(size = 8),
                   axis.title.y = ggplot2::element_text(angle = 90))+

     ecodata::theme_title()


  return(p)
}

attr(plot_seasonal_sst_anomaly_gridded,"report") <- c("MidAtlantic","NewEngland")
