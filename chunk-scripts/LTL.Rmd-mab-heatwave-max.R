
#EPU shapefile
mab_epu_sf <- ecodata::epu_sf %>% 
  filter(EPU %in% c("MAB"))

#Map line parameters
map.lwd <- 0.4

# Set lat/lon window for maps
xmin = -81
xmax = -66
ymin = 35.5
ymax = 43
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)
hw <- ecodata::hw_anom_gridded %>% 
  filter(EPU == "MAB")

mab_map <- 
  ggplot() +
  geom_tile(data =hw, aes(x = Longitude, y = Latitude,fill = Value)) +
  geom_sf(data = ecodata::coast, size = map.lwd) +
  geom_sf(data = mab_epu_sf, fill = "transparent", size = map.lwd) +
  scale_fill_gradient2(name = "Temp.\nAnomaly (C)",
                       low = scales::muted("blue"),
                       mid = "white",
                       high = scales::muted("red"),
                       limits = c(-5,7)) +
  coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  #facet_wrap(Season~.) +
  theme_map() +
  ggtitle("MAB heatwave anomaly (July 22, 2019)") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        legend.key = element_blank(),
        axis.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text=element_text(hjust=0),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(angle = 90))


mab_map 
