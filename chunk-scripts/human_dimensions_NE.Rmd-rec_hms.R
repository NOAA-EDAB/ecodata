
ecodata::rec_hms %>% 
  dplyr::filter(Region == "New England") %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x = Time, y = Value, color = Var))+
  ggplot2::geom_line(aes(x = Time, y = Value, color = Var))+
  ggplot2::ylab("Landings (N)")+
  ggplot2::xlab("")+ 
  ggplot2::ggtitle("Recreation Shark Landings")+
  ggplot2::theme(legend.title = element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
