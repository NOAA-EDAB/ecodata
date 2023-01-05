
#EPU shapefile
ne_epu_sf <- ecodata::epu_sf %>% 
  dplyr::filter(EPU %in% c("GOM","GB"))

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

bt<- bt %>% dplyr::mutate(Value = replace(Value, Value > 5, 5))
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
  ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  ggplot2::facet_wrap(Season~.) +
  ecodata::theme_map() +
  ggplot2::ggtitle("BT anomaly (2022)") +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        legend.key = element_blank(),
        axis.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text=element_text(hjust=0),
        axis.text = element_text(size = 8), 
        axis.title.y = element_text(angle = 90))+
  ecodata::theme_title()


bt_map 
