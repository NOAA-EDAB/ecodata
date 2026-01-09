#' plot dissolved oxygen from FishBOT
#'
#' plots number of days below hypoxia threshold
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10), passed from plot function
#' @param threshold numeric value. Threshold for hypoxia in mg/L. Default <= 5 mg/L
#'
#' @return ggplot object
#'
#' @export

plot_dissolved_oxygen <- function(shadedRegion = NULL,
                                  threshold = 5) {


  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report= 'MidAtlantic')

  #EPU shapefile
  ne_epu_sf <- ecodata::epu_sf

  ne_coast = sf::st_transform(ecodata::coast, crs = 4269)


  fix <- ecodata::dissolved_oxygen |>
    sf::st_as_sf(wkt = 'geometry', crs = 4269) |>
    dplyr::mutate(Year = format(Time, '%Y')) |>
    dplyr::filter(Year == max(Year) & Var == 'dissolved_oxygen') |>
    dplyr::mutate(below.hypoxia = Value < threshold) |>
    dplyr::group_by(Year,geometry) |>
    dplyr::summarise(hypoxia.count = sum(below.hypoxia,na.rm=T)) |>
    dplyr::mutate(hypoxia.count = ifelse(hypoxia.count == 0, NA, hypoxia.count))

  #transform epu and data to North American Albers Equal Area (EPSG: 5070)
  fix_flat = sf::st_transform(fix, crs = 3857)
  epu_flat = sf::st_transform(ne_epu_sf, crs = 3857)
  coast_flat = sf::st_transform(ne_coast,crs = 3857)

  p <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = coast_flat, size = setup$map.lwd) +
    ggplot2::geom_sf(data = epu_flat, fill = "azure1", size = setup$map.lwd) +
    ggplot2::geom_sf(data = fix_flat, ggplot2::aes(fill = hypoxia.count), color = 'black') +
    ggplot2::coord_sf(xlim = c(-77, -65), ylim = c(35.5,45), crs = 4326) +

    # ggplot2::facet_wrap(Var~.)+
    ecodata::theme_map() +
    ggplot2::scale_fill_viridis_c(name = paste0('# Days \n <',threshold,' mg/L'),na.value = 'lightgrey',option = 'inferno')+
    ggplot2::ggtitle(paste0('Hypoxia Events in ',max(fix$Year)))+
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ecodata::theme_title() +
    ecodata::theme_ts()+
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill= NA, linewidth=0.75),
                   legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = 11),
                   strip.background = ggplot2::element_blank(),
                   strip.text=ggplot2::element_text(hjust=0),
                   axis.text = ggplot2::element_text(size = 8),
                   axis.title.y = ggplot2::element_text(angle = 90))



  return(p)
}

# p <-
#   ggplot2::ggplot() +
#   ggplot2::geom_tile(data = sst, ggplot2::aes(x = Longitude, y = Latitude,fill = Value)) +
#   ggplot2::geom_sf(data = ecodata::coast, size = setup$map.lwd) +
#   ggplot2::geom_sf(data = ne_epu_sf, fill = "transparent", size = setup$map.lwd) +
#   ggplot2::scale_fill_gradient2(name = "Temp.\nAnomaly (C)",
#                                 low = scales::muted("blue"),
#                                 mid = "white",
#                                 high = scales::muted("red"),
#                                 limits = c(-5,5),
#                                 labels = c("<-5", "-2", "0", "2", ">5")) +
#   ggplot2::coord_sf(xlim = xlims, ylim = ylims) +
#   ggplot2::facet_wrap(Season~.) +
#   ecodata::theme_map() +
#   ggplot2::ggtitle("SST anomaly") +
#   ggplot2::xlab("Longitude") +
#   ggplot2::ylab("Latitude") +
#   ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.75),
#                  legend.key = ggplot2::element_blank(),
#                  axis.title = ggplot2::element_text(size = 11),
#                  strip.background = ggplot2::element_blank(),
#                  strip.text=ggplot2::element_text(hjust=0),
#                  axis.text = ggplot2::element_text(size = 8),
#                  axis.title.y = ggplot2::element_text(angle = 90))+
#
#    ecodata::theme_title()
