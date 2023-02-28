
ecodata::harborporpoise %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_line(aes(x=Time, y = totalest5y, color = "5yr rolling mean and CI"))+
  ggplot2::geom_line(aes(x=Time, y = totalest1y, color = "Annual estimates"), linetype = "dashed")+
  ggplot2::geom_line(aes(x=Time, y = pbr, color = "PBR"))+
  #ggplot2::geom_line(aes(x=year, y = PBR), color = "red")+
  ggplot2::geom_ribbon(aes(ymin = total5yLCI, ymax =total5yUCI, x = Time), fill = "blue", alpha = 0.2)+
  ggplot2::ggtitle("Harbor Porpoise Bycatch")+
  ggplot2::ylab("Estimated Bycatch (n)")+
  ggplot2::scale_color_manual(name = element_blank(), values = c('5yr rolling mean and CI' = 'blue', 
                                         'Annual estimates' = 'black',
                                         'PBR' = 'red'))+
  theme(legend.position = c(0.75, 0.8), 
        legend.text = element_text(size = 8), 
        legend.background = element_rect(colour = "transparent", fill = "transparent"))+
  ecodata::theme_ts()+
  ecodata::theme_title()
