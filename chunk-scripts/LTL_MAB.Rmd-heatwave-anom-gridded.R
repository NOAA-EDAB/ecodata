
#EPU shapefile
mab_epu_sf <- ecodata::epu_sf %>% 
  dplyr::filter(EPU %in% c("MAB"))

#Map line parameters
map.lwd <- 0.4

# Set lat/lon window for maps
xmin = -81
xmax = -66
ymin = 35.5
ymax = 43
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)
hw <- ecodata::heatwave_peak_date %>% 
  dplyr::filter(EPU == "MAB") %>% 
  dplyr::mutate(Value = replace(Value, Value > 4, 4))

mab_map <- 
    ggplot2::ggplot() +
  ggplot2::geom_tile(data =hw, aes(x = Longitude, y = Latitude,fill = Value)) +
  ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  ggplot2::geom_sf(data = mab_epu_sf, fill = "transparent", size = map.lwd) +
  ggplot2::scale_fill_gradientn(name = "Temp.\nAnomaly (C)", 
                                colours = c(scales::muted("blue"), "white",
                                            scales::muted("red"), "black"),
                                values = scales::rescale(c(-4,0,4,8)),
                                guide = "colorbar", limits=c(-4,8)) +
  
  # ggplot2::scale_fill_gradient2(name = "Temp Anomaly (C)",
  #                      low = scales::muted("blue"),
  #                      mid = "white",
  #                      midpoint = 0,
  #                      high = scales::muted("red"),
  #                      guide = "colourbar",
  #                      limits = c(-4,8)) +
  ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  #facet_wrap(Season~.) +
  ecodata::theme_map() +
  ggplot2::ggtitle("MAB heatwave anomaly (July 28, 2020)") +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        legend.key = element_blank(),
        axis.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text=element_text(hjust=0),
        axis.text = element_text(size = 8))


mab_map 
