
ecodata::harborporpoise %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x=Year, y = EST), color = "black")+
  ggplot2::geom_line(aes(x=Year, y = EST), color = "black")+
  ggplot2::geom_point(aes(x=Year, y = PBR), color = "red")+
  ggplot2::geom_line(aes(x=Year, y = PBR), color = "red")+
  ggplot2::geom_ribbon(aes(ymin = lo95ci, ymax = up95ci, x = Year), alpha = 0.3)+
  ggplot2::ggtitle("Harbor Porpoise Byctach")+
  ggplot2::ylab("Estimated Bycatch (n)")+
  ecodata::theme_ts()
