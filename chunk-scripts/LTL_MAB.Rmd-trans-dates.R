
dat<- readxl::read_xlsx(here::here("data-raw/nes_half_deg_stand_area_grid_trans_local_means_and_plus_minus_means.xlsx"))

dat2<- dat %>% 
  dplyr::select(1:7) %>% 
  tidyr::separate(local, into = c("lat", "lon"), "_") %>% 
  mutate(lat = as.numeric(lat), 
         lon = as.numeric(lon)) %>% 
  group_by(year) %>% 
  summarise(fall = mean(fall, na.rm = TRUE  ), 
            spring = mean(spring)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(fall, spring), names_to = "season", values_to = "value")

dat2 %>% ggplot2::ggplot(aes(x = year, y = value, group = season))+
  geom_point()+
  geom_line()+
  #coord_flip()+
  ylab("DOY")+
  xlab("Time")+
  ggtitle("Seasonal Transition Dates")+
  ecodata::geom_gls()+
  #ggplot2::theme(legend.title = "Season")+
  ecodata::theme_title()+
  scale_y_reverse()+
  ecodata::theme_ts()
