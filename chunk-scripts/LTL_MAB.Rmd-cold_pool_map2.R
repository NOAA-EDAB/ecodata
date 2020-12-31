
cpsf26<- ecodata::cold_pool_sf %>% 
  mutate(Time = c(1993:2018)) %>% 
  filter(Time == 2018)
cpsf<- ecodata::cold_pool_sf %>% 
  mutate(Time = c(1993:2018)) %>% 
  filter(!Time == 2018)

#Map line parameters
map.lwd <- 0.4

# Set lat/lon window for maps
xmin = -80
xmax = -68
ymin = 35.5
ymax = 43
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)

cp_map <- 
  ggplot2::ggplot() +
  #ggplot2::geom_tile(data =hw, aes(x = Longitude, y = Latitude,fill = Value)) +
  ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  ggplot2::geom_sf(data = cpsf, alpha = 0.1, color = "red", size = 1)  +
  ggplot2::geom_sf(data = cpsf26, fill = "transparent", color = "red", size = 1)+
  
  ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  #ggplot2::scale_fill_gradient2(name = "Temp.\nAnomaly (C)",
  #                     low = scales::muted("blue"),
  #                     mid = "white",
  #                     high = scales::muted("red"),
  #                     limits = c(-4,4)) +
  
  #facet_wrap(Season~.) +
  ecodata::theme_map() +
  ggplot2::facet_wrap(~Time)+
  ggplot2::ggtitle("Cold Pool Area") +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        legend.key = element_blank(),
        axis.title = element_text(size = 11),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text=element_text(hjust=0),
        axis.text = element_blank(),
        axis.title.y = element_text(angle = 90))


cp_map 
