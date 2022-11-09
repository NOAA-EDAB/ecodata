
new<- read.csv(here::here("data-raw/SST_ltm9220.csv")) %>% 
  dplyr::filter(EPU == c("GB", "GOM")) %>% 
  dplyr::mutate(ltm = c("91-20")) %>% 
  dplyr::select(-X)
ecodata::seasonal_oisst_anom %>%
  dplyr::mutate(ltm = c("82-10")) %>% 
  dplyr::filter(EPU  == c("GB", "GOM")) %>%
  rbind(new) %>% 
  tidyr::separate(Var, into = c("Var", "OI","SST", "anom")) %>% 
  dplyr::mutate(Var = recode(Var, "winter" = "Winter"), 
         Var = recode(Var, "spring" = "Spring"),
         Var = recode(Var, "summer" = "Summer"),
         Var = recode(Var, "fall" = "Fall")) %>% 
  ggplot(aes(x= Time, y = Value, color = ltm))+
  geom_point()+
  geom_line()+
  ecodata::geom_gls()+
  facet_grid(EPU~Var)+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0),
                 plot.title = element_text(size = 12))+
  ecodata::theme_title()
