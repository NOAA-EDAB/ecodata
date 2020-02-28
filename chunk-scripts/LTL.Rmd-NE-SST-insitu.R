
#EPU shapefile
ne_epu_sf <- ecodata::epu_sf %>% 
  filter(EPU %in% c("GOM","GB"))

#Map line parameters
map.lwd <- 0.4

# Set lat/lon window for maps
xmin = -73
xmax = -65
ymin = 39
ymax = 45
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)
sst <- ecodata::seasonal_sst_anomaly_gridded 

sst$Season <- factor(sst$Season, levels = c("Winter",
                                            "Spring",
                                            "Summer",
                                            "Fall"))

sst<- sst %>% mutate(Value = replace(Value, Value > 4, 4))
sst_map <- 
  ggplot() +
  geom_tile(data = sst, aes(x = Longitude, y = Latitude,fill = Value)) +
  geom_sf(data = coast, size = map.lwd) +
  geom_sf(data = ne_epu_sf, fill = "transparent", size = map.lwd) +
  scale_fill_gradient2(name = "Temp.\nAnomaly (°C)",
                       low = scales::muted("blue"),
                       mid = "white",
                       high = scales::muted("red"),
                       limits = c(-4,4), 
                       labels = c("<-4", "-2", "0", "2", ">4")) +
  coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  facet_wrap(Season~.) +
  theme_map() +
  ggtitle("SST anomaly (2019)") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        legend.key = element_blank(),
        axis.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text=element_text(hjust=0),
        axis.text = element_text(size = 8), 
        axis.title.y = element_text(angle = 90))


sst_map 
