
blue<-ecodata::blue_runner %>% 
  separate(Var, c("Var", "Pos"), "L") %>% 
  spread(., Pos, Value) %>% 
  rename(Lat = at, 
         Lon = on) %>% 
  mutate(Var = recode(Var, 
                      "Positive Blue Runner Tows 2001 - 2010 - " = "2001-2010", 
                      "Positive Blue Runner Tows before 2001 - " = "Prior to 2000",
                      "Positive Blue Runner Tows since 2010 - " = "Since 2010"))
  
#EPU shapefile
epu_sf <- ecodata::epu_sf %>% 
  filter(EPU %in% c("GOM","GB", "MAB"))

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
  ggplot() +
  geom_sf(data = coast, size = map.lwd) +
  geom_sf(data = epu_sf, fill = "transparent", size = map.lwd) +
  geom_point(data = blue, aes(x = Lon, y = Lat, color = Var))+
  coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  theme_map() +
  ggtitle("Blue Runner Presence") +
  xlab("") +
  ylab("") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        legend.key = element_blank(),
        axis.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text=element_text(hjust=0),
        axis.text = element_text(size = 8), 
        legend.title = element_blank())

blue_map
