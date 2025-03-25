
# Set lat/lon window for maps
xmin = -77
xmax = -65
ymin = 36
ymax = 45
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)
sst <- ecodata::seasonal_oisst_anom_gridded 

sst$Season <- factor(sst$Season, levels = c("Winter",
                                            "Spring",
                                            "Summer",
                                            "Fall"))

sst<- sst %>% dplyr::mutate(Value = replace(Value, Value > 5, 5))
epu_sf <- ecodata::epu_sf[ecodata::epu_sf$EPU != "SS",]

ggplot2::ggplot() +
  ggplot2::geom_tile(data = sst, aes(x = Longitude, y = Latitude, fill = Value)) +
  ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  ggplot2::geom_sf(data = epu_sf, fill = "transparent", size = map.lwd) +
  ggplot2::scale_fill_gradient2(name = "Temp.\nAnomaly (C)",
                       low = scales::muted("blue"),
                       mid = "white",
                       high = scales::muted("red"),
                       limits = c(-5,5), 
                       labels = c("<-5", "-2", "0", "2", ">5")) +
  ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  ggplot2::facet_wrap(Season~.) +
  ecodata::theme_map() +
  ggplot2::ggtitle("SST anomaly (2022)") +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        legend.key = element_blank(),
        axis.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text=element_text(hjust=0),
        axis.text = element_text(size = 8))+
  ecodata::theme_title()
