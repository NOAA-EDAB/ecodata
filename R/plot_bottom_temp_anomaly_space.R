#' plot botttom temperature anomaly
#'
#' @param season Character string. Season to plot. (Default = NULL, plot all seasons)
#' @param region Character vector. Regional EPUs ("GB","MAB") to overly on figure. (Default = NULL, use all)
#'
#'
#' @return ggplot object
#'
#' @export

plot_bottom_temp_anomaly_space <- function(season = NULL,
                                     region = NULL) {

  if(is.null(region)){
    region <- c("GOM","GB")
  }
  if(is.null(season)) {
    season <- c("Winter","Spring","Summer","Fall")
  }


  #EPU shapefile
  ne_epu_sf <- ecodata::epu_sf %>%
    dplyr::filter(EPU %in% region)

  #Map line parameters
  map.lwd <- 0.4

  # Set lat/lon window for maps
  xmin = -73
  xmax = -66
  ymin = 39
  ymax = 44.1
  xlims <- c(xmin, xmax)
  ylims <- c(ymin, ymax)
  bt <- ecodata::seasonal_bt_anomaly_gridded

  bt$Season <- factor(bt$Season, levels = c("Winter",
                                            "Spring",
                                            "Summer",
                                            "Fall"))

  bt<- bt %>% dplyr::mutate(Value = replace(Value, Value > 5, 5)) %>%
    dplyr::filter(Season %in% season)

  bt_map <-
    ggplot2::ggplot() +
    ggplot2::geom_tile(data = bt, aes(x = Longitude, y = Latitude,fill = Value)) +
    ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
    ggplot2::geom_sf(data = ne_epu_sf, fill = "transparent", size = map.lwd) +
    ggplot2::scale_fill_gradient2(name = "Temp.\nAnomaly (C)",
                                  low = scales::muted("blue"),
                                  mid = "white",
                                  high = scales::muted("red"),
                                  limits = c(-5,5),
                                  labels = c("<-5", "-2", "0", "2", ">5")) +
    ggplot2::coord_sf( xlim = xlims, ylim = ylims) +
    ggplot2::facet_wrap(Season~.) +
    ecodata::theme_map() +
    ggplot2::ggtitle("BT anomaly (2022)") +
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


  return(p=bt_map)
}
