
ecodata::rec_hms %>% 
  filter(EPU == "MAB") %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x = Time, y = Value, color = Var))+
  ggplot2::geom_line(aes(x = Time, y = Value, color = Var))+
  ggplot2::ylab("Landings (N)")+
  ggplot2::xlab(element_blank())+
  ggplot2::theme(legend.title = element_blank())+
  ecodata::theme_ts()
