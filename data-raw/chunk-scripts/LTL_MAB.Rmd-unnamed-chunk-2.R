
sst<- ecodata::ches_bay_sst

map.lwd <- 0.4

# Set lat/lon window for maps
xmin = -78
xmax = -74
ymin = 36
ymax = 40
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)
#sst <- ecodata::seasonal_oisst_anom_gridded
crs<- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
sst$Season <- factor(sst$Season, levels = c("Winter",
                                            "Spring",
                                            "Summer",
                                            "Fall"))

sst<- sst %>% dplyr::mutate(Value = replace(Value, Value > 5, 5))
sst_map <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  ggplot2::scale_fill_gradient2(name = "Temp.\nAnomaly (C)",
                                low = scales::muted("blue"),
                                mid = "white",
                                high = scales::muted("red"),
                                limits = c(-4,4),
                                labels = c("<-5", "-2.5", "0", "2.5", ">5")) +
  ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +

  ggplot2::geom_tile(data = sst, aes(x = Latitude, y = Longitude,fill = Value)) +
  ggplot2::facet_wrap(Season~.) +
  ecodata::theme_map() +
  ggplot2::ggtitle("Chesapeake Bay SST anomaly (2021)") +
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


sst_map
