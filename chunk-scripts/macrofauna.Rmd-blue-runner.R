
blue<-ecodata::blue_runner %>% 
  tidyr::separate(Var, c("Var", "Pos"), "L") %>% 
  tidyr::spread(., Pos, Value) %>% 
  dplyr::rename(Lat = at, 
         Lon = on) %>% 
  dplyr::mutate(Var = recode(Var,
                      "Positive Blue Runner Tows before 2001 - " = "Prior to 2000",
                      "Positive Blue Runner Tows 2001 - 2010 - " = "2001-2010", 
                      "Positive Blue Runner Tows since 2010 - " = "Since 2010"))
blue$Var <- factor(blue$Var, levels = c("Prior to 2000", "2001-2010", "Since 2010"))
  
#EPU shapefile
epu_sf <- ecodata::epu_sf %>% 
  dplyr::filter(EPU %in% c("GOM","GB", "MAB"))

#Map line parameters
map.lwd <- 0.4

# Set lat/lon window for maps
xmin = -77
xmax = -65
ymin = 35
ymax = 45
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)

## Map plotting blue runner
blue_map <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  ggplot2::geom_sf(data = epu_sf, fill = "transparent", size = map.lwd) +
  ggplot2::geom_point(data = blue, aes(x = Lon, y = Lat, color = Var, shape = Var))+
  ggplot2::scale_shape_manual(values=c(16, 3, 17))+
  ggplot2::scale_color_manual(values = c("blue", "black", "red"))+
  ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  ecodata::theme_map() +
  ggplot2::ggtitle("Blue Runner Presence") +
  ggplot2::xlab("") +
  ggplot2::ylab("") +
  ggplot2::theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        legend.key = element_blank(),
        axis.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text=element_text(hjust=0),
        axis.text = element_text(size = 8), 
        legend.title = element_blank(), 
        legend.position = c(0.6, 0.15))

blue_map
