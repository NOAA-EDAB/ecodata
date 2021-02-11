
ecodata::rec_hms %>% 
  dplyr::filter(Region == "Mid-Atlantic") %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x = Time, y = Value, color = Var))+
  ggplot2::geom_line(aes(x = Time, y = Value, color = Var))+
  ggplot2::ylab("Landings (N)")+
  ggplot2::xlab("")+ 
  ggplot2::theme(legend.title = element_blank())+
  ecodata::theme_ts()
