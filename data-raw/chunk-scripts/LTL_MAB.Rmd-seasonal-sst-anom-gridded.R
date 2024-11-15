
annotation_custom2 <- function (data, ...)
{
  layer(data = data,
        stat = "identity",
        position = "identity",
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = FALSE,
        params = list(...))
}

#EPU shapefile
mab_epu_sf <- ecodata::epu_sf %>% 
  dplyr::filter(EPU %in% c("MAB")) 

#Map line parameters
map.lwd <- 0.4

# Set lat/lon window for maps
xmin = -81
xmax = -66
ymin = 35.5
ymax = 43
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)
sst <- ecodata::seasonal_oisst_anom_gridded 

sst$Season <- factor(sst$Season, levels = c("Winter",
                                            "Spring",
                                            "Summer",
                                            "Fall"))
# ma_anom<-ecodata::seasonal_oisst_anom
# 
# ma_anom$Var <- factor(ma_anom$Var, levels= c("Winter","Spring","Summer","Fall"))
sst<- sst %>% dplyr::mutate(Value = replace(Value, Value > 5, 5))

#functions
sst_map <- function(season){
  ggplot2::ggplot(sst %>%
                    dplyr::filter(Season == season)) +
  ggplot2::geom_tile(aes(x = Longitude, y = Latitude,fill = Value)) +
  ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  ggplot2::geom_sf(data = mab_epu_sf, fill = "transparent", size = map.lwd) +
  ggplot2::scale_fill_gradient2(name = "Temp.\nAnomaly (C)",
                       low = scales::muted("blue"),
                       mid = "white",
                       high = scales::muted("red"),
                       limits = c(-5,5),
                       labels = c("<-5", "-2", "0", "2", ">5")) +
  ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  #ggplot2::facet_wrap(Season~.) +
  ecodata::theme_map() +
  ggplot2::ggtitle(season) +
  ggplot2::xlab(element_blank()) +
  ggplot2::ylab(element_blank()) +
  ggplot2::theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        legend.key = element_blank(),
        axis.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text=element_text(hjust=0),
        axis.text = element_text(size = 8), 
        axis.title.y = element_text(angle = 90))+
  ecodata::theme_title()
}

season_anom <- function(season){
  ggplot2::ggplotGrob( ecodata::seasonal_oisst_anom %>% 
                              dplyr::filter(EPU == "MAB",
                                     stringr::str_detect(Var, season)) %>% 
                              dplyr::mutate(hline = mean(Value)) %>% 
                              ggplot2::ggplot(aes(x = Time, y = Value)) +
                              ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
                                       xmin = x.shade.min , xmax = x.shade.max,
                                       ymin = -Inf, ymax = Inf) +
                              ggplot2::geom_line() +
                              ggplot2::geom_point() +
                              ecodata::geom_gls(alpha = trend.alpha + 0.25) +
                              ggplot2::ylab("SST anomaly (C)")+
                              ggplot2::xlab(element_blank())+
                              ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
                              ggplot2::geom_hline(aes(yintercept = hline)) +
                              ecodata::theme_ts()+
                              ggplot2::theme(axis.title = element_text(size = 6),
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
}

# create anomaly grobs
winter_anom <- season_anom(season = "Winter")

spring_anom <- season_anom(season = "Spring")

summer_anom <- season_anom(season = "Summer")

fall_anom <- season_anom(season = "Fall")

# create plots for each season
winter <- sst_map(season = "Winter") +
  annotation_custom2(grob = winter_anom,  xmin=-82, xmax=-73.75,
                     ymin=39, ymax=43.5, data = data.frame(Season = "Winter"))

spring <- sst_map(season = "Spring") +
  annotation_custom2(grob = spring_anom,  xmin=-82, xmax=-73.75,
                   ymin=39, ymax=43.5, data = data.frame(Season = "Spring"))

summer <- sst_map(season = "Summer") +
  annotation_custom2(grob = summer_anom,  xmin=-82, xmax=-73.75,
                   ymin=39, ymax=43.5, data = data.frame(Season = "Summer"))

fall <- sst_map(season = "Fall") +
  annotation_custom2(grob = fall_anom,  xmin=-82, xmax=-73.75,
                   ymin=39, ymax=43.5, data = data.frame(Season = "Fall"))

# arrange
ggpubr::ggarrange(winter, spring, summer, fall,
                  ncol = 2,
                  nrow = 2,
                  common.legend = TRUE,
                  legend = "right") %>%
  ggpubr::annotate_figure(top = "SST anomaly (2022)", 
                          left = "Latitude", 
                          bottom = "Longitude")
