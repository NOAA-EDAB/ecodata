
hms10<- ecodata::HMS_species_distribution %>%
  tidyr::separate(Var, into = c("Var", "species", "season"), sep = "_") %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value) %>% 
  dplyr::filter(Time == 2010) %>% 
  dplyr::rename("x_start" = wlon, 
                "y_start" = wlat) %>% 
  dplyr::select(!Time)

hms<-ecodata::HMS_species_distribution %>%
  tidyr::separate(Var, into = c("Var", "species", "season"), sep = "_") %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value) %>% 
  dplyr::filter(Time == 2017) %>% 
  dplyr::rename("x_end" = wlon, 
                "y_end" = wlat) %>% 
  dplyr::select(!Time) %>% 
  dplyr::left_join(hms10) %>% 
  dplyr::mutate(season = dplyr::recode(season, fall = "Fall", 
                                       winter = "Winter", 
                                       spring = "Spring", 
                                       summer = "Summer"))
hms$season <- factor(hms$season,levels = c("Winter","Spring", 
                                           "Summer", "Fall"))
epu_sf <- ecodata::epu_sf %>%  
  dplyr::filter(EPU %in% c("GOM","GB", "MAB"))
map.lwd <- 0.4 
xmin = -77 
xmax = -65 
ymin = 35 
ymax = 45
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax) 
hms_map <- hms %>% ggplot2::ggplot() + 
  ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  ggplot2::geom_sf(data = epu_sf, fill = "transparent", size = map.lwd) + 
  geom_segment( mapping = aes(x = x_start, y = y_start, 
                              xend = x_end, yend = y_end, color = species),  
                              arrow = grid::arrow(length = grid::unit(1, "mm")))+

  #ggplot2::scale_shape_manual(values=c(16, 3, 17))+ 
  #ggplot2::scale_color_manual(values = c("blue", "black", "red"))+ 
  ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) + 
  ecodata::theme_map() + 
  ggplot2::ggtitle("Protected Species Distibution Shifts") + 
  ggplot2::xlab("") + 
  ggplot2::ylab("") +
  scale_x_continuous(breaks=c(-76,-72, -68) )+
  ggplot2::theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                #legend.key = element_blank(),
                axis.title = element_text(size = 11),
                strip.background = element_blank(),
                strip.text=element_text(hjust=0),
                axis.text = element_text(size = 8),
                legend.title = element_blank())+#,
                #legend.position = c(0.6, 0.15)) +
  ggplot2::facet_wrap(~season)
hms_map
