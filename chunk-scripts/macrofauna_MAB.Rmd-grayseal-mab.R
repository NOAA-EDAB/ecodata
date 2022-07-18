
ecodata::grayseal %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_line(aes(x=Time, y = totalest5y, color = "5yr rolling mean and confidence interval"))+
  #ggplot2::geom_line(aes(x=year, y = totalest1y, color = "Annual estimates"), linetype = "dashed")+
  ggplot2::geom_line(aes(x=Time, y = pbr, color = "PBR"))+
  #ggplot2::geom_line(aes(x=year, y = PBR), color = "red")+
  ggplot2::geom_ribbon(aes(ymin = total5yLCI, ymax =total5yUCI, x = Time), fill = "blue", alpha = 0.2)+
  ggplot2::ggtitle("Gray Seal Byctach")+
  ggplot2::ylab("Estimated Bycatch (n)")+
  ggplot2::xlab(element_blank())+
  ggplot2::scale_color_manual(name = element_blank(), values = c('5yr rolling mean and confidence interval' = 'blue', 
   #                                      'NE Annual estimates' = 'black',
                                         'PBR' = 'red'))+
  theme(legend.position = c(0.3, 0.85))+
  ecodata::theme_ts()+
  ecodata::theme_title()
