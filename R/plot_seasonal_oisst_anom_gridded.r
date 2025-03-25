#' plot seasonal_oisst_anom_gridded
#'
#' @param season Character string. Season to plot. (Default = NULL, plot all seasons)
#' @param region Character vector. Regional EPUs ("GB","MAB") to overly on figure. (Default = NULL, use all)
#' @param scale character string. celsius or fahrenheit. Default = "celsius"
#'
#'
#' @return ggplot object
#'
#' @export

plot_seasonal_oisst_anom_gridded <- function(shadedRegion = NULL,
                                              report = "MidAtlantic",
                                              scale = "celsius") {


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

  sst <- ecodata::seasonal_oisst_anom_gridded |>
    dplyr::mutate(Season = factor(Season, levels = c("Winter",
                                                     "Spring",
                                                     "Summer",
                                                     "Fall")))


  if (scale == "fahrenheit") {
    # convert celsius anomaly to fahrenheit anomaly
    sst <- sst |>
      dplyr::mutate(Value = (9/5)*Value )
    label <- "Temp.\nAnomaly (\u00B0F)"
    breaks <- c(-9.0, -4.5,  0.0,  4.5,  9.0)
    labelLegend <- c("<-9", "-4.5", "0", "4.5", ">9")
    limits <- c(-9,9)
    maxVal <- 9
    midpoint <- 0
  } else {
    label <- "Temp.\nAnomaly (\u00B0C)"
    breaks <- c(-5,-2.5,0,2.5,5)
    labelLegend <- c("<-5", "-2.5", "0", "2.5", ">5")
    limits <- c(-5,5)
    maxVal <- 5
    midpoint <- 0
  }

  sst <- sst |> dplyr::mutate(Value = replace(Value, Value > maxVal, maxVal))


  # filter by season
  # sst <- sst %>%
  #   dplyr::filter(Season %in% season)

  p <-
    ggplot2::ggplot() +
    ggplot2::geom_tile(data = sst, ggplot2::aes(x = Longitude, y = Latitude,fill = Value)) +
    ggplot2::geom_sf(data = ecodata::coast, size = setup$map.lwd) +
    ggplot2::geom_sf(data = ne_epu_sf, fill = "transparent", size = setup$map.lwd) +
    ggplot2::scale_fill_gradient2(name = label,
                                  low = scales::muted("blue"),
                                  mid = "white",
                                  high = scales::muted("red"),
                                  limits = limits,
                                  labels = labelLegend,
                                  breaks = breaks,
                                  midpoint = midpoint) +
    ggplot2::coord_sf(xlim = xlims, ylim = ylims) +
    ggplot2::facet_wrap(Season~.) +
    ecodata::theme_map() +
    ggplot2::ggtitle("SST anomaly") +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, linewidth=0.75),
                   legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = 11),
                   strip.background = ggplot2::element_blank(),
                   strip.text=ggplot2::element_text(hjust=0),
                   axis.text = ggplot2::element_text(size = 8),
                   axis.title.y = ggplot2::element_text(angle = 90))+

     ecodata::theme_title()


  return(p)
}

attr(plot_seasonal_oisst_anom_gridded,"report") <- c("MidAtlantic","NewEngland")
