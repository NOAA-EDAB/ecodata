
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

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
sst <- seasonal_sst_anomaly_gridded 

sst$Season <- factor(sst$Season, levels = c("Winter",
                                            "Spring",
                                            "Summer",
                                            "Fall"))
sst<- sst %>% mutate(Value = replace(Value, Value > 4, 4))
sst_map <- 
  ggplot() +
  geom_tile(data = sst, aes(x = Longitude, y = Latitude,fill = Value)) +
  geom_sf(data = ecodata::coast, size = map.lwd) +
  geom_sf(data = mab_epu_sf, fill = "transparent", size = map.lwd) +
  scale_fill_gradient2(name = "Temp.\nAnomaly (C)",
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

winter_anom <-  ggplotGrob( seasonal_oisst_anom %>% 
                              filter(EPU == "MAB",
                                     str_detect(Var, "winter")) %>% 
                              mutate(hline = mean(Value)) %>% 
                              ggplot(aes(x = Time, y = Value)) +
                              annotate("rect", fill = shade.fill, alpha = shade.alpha,
                                       xmin = x.shade.min , xmax = x.shade.max,
                                       ymin = -Inf, ymax = Inf) +
                              geom_line() +
                              geom_point() +
                              geom_gls(alpha = trend.alpha + 0.25) +
                              ylab("SST anomaly (C)")+
                              xlab(element_blank())+
                              scale_x_continuous(expand = c(0.01, 0.01)) +
                              geom_hline(aes(yintercept = hline)) +
                              theme_ts()+
                              theme(axis.title = element_text(size = 6),
                                    axis.text = element_text(size = 6),
                                    panel.background = element_rect(fill = "transparent"), 
                                    plot.background = element_rect(fill = "transparent", color = NA), 
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(), 
                                    legend.background = element_rect(fill = "transparent"), 
                                    legend.box.background = element_rect(fill = "transparent"), 
                                    legend.key = element_rect(fill = "transparent", colour = NA), 
                                    axis.line = element_blank(),
                                    panel.border = element_blank())
)

spring_anom <-  ggplotGrob( seasonal_oisst_anom %>% 
                              filter(EPU == "MAB",
                                     str_detect(Var, "spring")) %>% 
                              mutate(hline = mean(Value)) %>% 
                              ggplot(aes(x = Time, y = Value)) +
                              annotate("rect", fill = shade.fill, alpha = shade.alpha,
                                       xmin = x.shade.min , xmax = x.shade.max,
                                       ymin = -Inf, ymax = Inf) +
                              geom_line() +
                              geom_point() +
                              geom_gls(alpha = trend.alpha + 0.25) +
                              ylab("SST anomaly (C)")+
                              xlab(element_blank())+
                              scale_x_continuous(expand = c(0.01, 0.01)) +
                              geom_hline(aes(yintercept = hline)) +
                              theme_ts()+
                              theme(axis.title = element_text(size = 6),
                                    axis.text = element_text(size = 6),
                                    panel.background = element_rect(fill = "transparent"), 
                                    plot.background = element_rect(fill = "transparent", color = NA),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(), 
                                    legend.background = element_rect(fill = "transparent"), 
                                    legend.box.background = element_rect(fill = "transparent"), 
                                    legend.key = element_rect(fill = "transparent", colour = NA), 
                                    axis.line = element_blank(),
                                    panel.border = element_blank())
)

summer_anom <-  ggplotGrob( seasonal_oisst_anom %>% 
                              filter(EPU == "MAB",
                                     str_detect(Var, "summer")) %>% 
                              mutate(hline = mean(Value)) %>% 
                              ggplot(aes(x = Time, y = Value)) +
                              annotate("rect", fill = shade.fill, alpha = shade.alpha,
                                       xmin = x.shade.min , xmax = x.shade.max,
                                       ymin = -Inf, ymax = Inf) +
                              geom_line() +
                              geom_point() +
                              geom_gls(alpha = trend.alpha + 0.25) +
                              ylab("SST anomaly (C)")+
                              xlab(element_blank())+
                              scale_x_continuous(expand = c(0.01, 0.01)) +
                              geom_hline(aes(yintercept = hline)) +
                              theme_ts()+
                              theme(axis.title = element_text(size = 6),
                                    axis.text = element_text(size = 6),
                                    panel.background = element_rect(fill = "transparent"),
                                    plot.background = element_rect(fill = "transparent", color = NA),
                                    panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank(), 
                                    legend.background = element_rect(fill = "transparent"),
                                    legend.box.background = element_rect(fill = "transparent"), 
                                    legend.key = element_rect(fill = "transparent", colour = NA), 
                                    axis.line = element_blank(),
                                    panel.border = element_blank())
)

fall_anom <-  ggplotGrob( seasonal_oisst_anom %>% 
                            filter(EPU == "MAB",
                                   str_detect(Var, "fall")) %>% 
                            mutate(hline = mean(Value)) %>% 
                            ggplot(aes(x = Time, y = Value)) +
                            annotate("rect", fill = shade.fill, alpha = shade.alpha,
                                     xmin = x.shade.min , xmax = x.shade.max,
                                     ymin = -Inf, ymax = Inf) +
                            geom_line() +
                            geom_point() +
                            geom_gls(alpha = trend.alpha + 0.25) +
                            ylab("SST anomaly (C)")+
                            xlab(element_blank())+
                            scale_x_continuous(expand = c(0.01, 0.01)) +
                            geom_hline(aes(yintercept = hline)) +
                            theme_ts()+
                            theme(axis.title = element_text(size = 6),
                                  axis.text = element_text(size = 6),
                                  panel.background = element_rect(fill = "transparent"), 
                                  plot.background = element_rect(fill = "transparent", color = NA), 
                                  panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank(), 
                                  legend.background = element_rect(fill = "transparent"), 
                                  legend.box.background = element_rect(fill = "transparent"),
                                  legend.key = element_rect(fill = "transparent", colour = NA),
                                  axis.line = element_blank(),
                                  panel.border = element_blank())
)

sst_map + 
  annotation_custom2(grob = winter_anom,  xmin=-82, xmax=-73.75,
                     ymin=39, ymax=43.5, data = data.frame(Season = "Winter")) +
  annotation_custom2(grob = spring_anom,  xmin=-82, xmax=-73.75,
                     ymin=39, ymax=43.5, data = data.frame(Season = "Spring")) +
  annotation_custom2(grob = summer_anom,  xmin=-82, xmax=-73.75,
                     ymin=39, ymax=43.5, data = data.frame(Season = "Summer")) +
  annotation_custom2(grob = fall_anom,  xmin=-82, xmax=-73.75,
                     ymin=39, ymax=43.5, data = data.frame(Season = "Fall"))
