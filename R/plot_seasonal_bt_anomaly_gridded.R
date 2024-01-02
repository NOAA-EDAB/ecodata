#' plot botttom temperature anomaly
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10), passed from plot function
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland"), passed from plot function
#'
#'
#' @return ggplot object
#'
#' @export

plot_seasonal_bt_anomaly_gridded <- function(shadedRegion=c(2012,2022),
                                             report = "MidAtlantic") {


  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
    xmin = -81
    xmax = -67
    ymin = 36.5
    ymax = 43
    xlims <- c(xmin, xmax)
    ylims <- c(ymin, ymax)
  } else {
    filterEPUs <- c("GB", "GOM")
    xmin = -73
    xmax = -66
    ymin = 39
    ymax = 44.1
    xlims <- c(xmin, xmax)
    ylims <- c(ymin, ymax)

  }

  # Set lat/lon window for maps



  #EPU shapefile
  ne_epu_sf <- ecodata::epu_sf |>
    dplyr::filter(EPU %in% filterEPUs)

  fix <- ecodata::seasonal_bt_anomaly_gridded |>
    dplyr::mutate (Season = factor(Season, levels = c("Winter",
                                                         "Spring",
                                                         "Summer",
                                                         "Fall"))) |>
    dplyr::mutate(Value = replace(Value, Value > 5, 5))

  p <- ggplot2::ggplot(data = fix) +
    ggplot2::geom_tile(ggplot2::aes(x = Longitude, y = Latitude, fill = Value)) +
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
    ggplot2::ggtitle("BT anomaly") +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, linewidth=0.75),
                   legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = 11),
                   strip.background = ggplot2::element_blank(),
                   strip.text=ggplot2::element_text(hjust=0),
                   axis.text = ggplot2::element_text(size = 8),
                   axis.title.y = ggplot2::element_text(angle = 90))+
    ecodata::theme_title() +
    ecodata::theme_ts()


  return(p)
}
