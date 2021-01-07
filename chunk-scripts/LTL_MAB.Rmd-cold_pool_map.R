
cpsf26<- ecodata::cold_pool_sf %>% 
  mutate(Time = c(1:26)) %>% 
  filter(Time == 26)
cpsf<- ecodata::cold_pool_sf %>% 
  mutate(Time = c(1:26)) %>% 
  filter(!Time == 26)

#Map line parameters
map.lwd <- 0.4

# Set lat/lon window for maps
xmin = -78
xmax = -69
ymin = 35.5
ymax = 43
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)

cp_map <- 
  ggplot2::ggplot() +
  #ggplot2::geom_tile(data =hw, aes(x = Longitude, y = Latitude,fill = Value)) +
  ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  ggplot2::geom_sf(data = cpsf, alpha = 0.1)  +
  ggplot2::geom_sf(data = cpsf26, fill = "transparent", color = "black", size = 1)+
  
  ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  #ggplot2::scale_fill_gradient2(name = "Temp.\nAnomaly (C)",
  #                     low = scales::muted("blue"),
  #                     mid = "white",
  #                     high = scales::muted("red"),
  #                     limits = c(-4,4)) +
  
  #facet_wrap(Season~.) +
  ecodata::theme_map() +
  ggplot2::ggtitle("Cold Pool Area") +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        legend.key = element_blank(),
        axis.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text=element_text(hjust=0),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(angle = 90))


annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) 
{
  layer( stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

area<- ecodata::cold_pool_sf %>%
  mutate(Time = c(1993:2018)) 
area2<- data.frame(Area = st_area(area))%>% 
  mutate(Time = c(1993:2018)) %>% 
  separate(Area, c("Value", "Units"), sep = " ") %>%
  mutate(Value = as.numeric(Value), 
         hline = mean(Value)) 

area_ts <-  ggplot2::ggplotGrob(area2 %>% 
                                      ggplot2::ggplot() +   
                                      ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
                                      ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
                                      ggplot2::geom_hline(aes(yintercept = hline),
                                                          size = hline.size,
                                                          alpha = hline.alpha,
                                                          linetype = hline.lty)+
                                      ggplot2::scale_x_continuous(breaks = c(1995, 2000, 2005, 2010, 2015),
                                                                  expand = c(0.01, 0.01)) +
                                      ggplot2::ggtitle("")+
                                      ggplot2::ylab("Area (m^2)") +
                                      ggplot2::xlab("")+
                                      ecodata::theme_ts()+
                              ggplot2::theme(
                                   axis.text.y = element_text(size = 6),
                                   axis.text.x = element_text(size = 6),
                                    panel.background = element_rect(fill = "transparent"),
                                    plot.background = element_rect(fill = "transparent", color = NA),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    legend.background = element_rect(fill = "transparent"),
                                    legend.box.background = element_rect(fill = "transparent"),
                                    legend.key = element_rect(fill = "transparent", colour = NA),
                                    axis.line = element_blank())#,
                                    #panel.border = element_blank())
)

cp_map +
  annotation_custom(grob = area_ts,  xmin=-78.5, xmax=-71.75,
                     ymin=40.95, ymax=43.75)
