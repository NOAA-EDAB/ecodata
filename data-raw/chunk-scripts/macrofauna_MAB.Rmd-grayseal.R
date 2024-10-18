
ribbon<- ecodata::grayseal %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value)  
  
ecodata::grayseal %>% 
  dplyr::filter(Var %in% c("pbr", "totalest5y", "totalest1y")) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_line(aes(x = Time, y = Value, linetype = Var, color = Var))+
  ggplot2::geom_ribbon(data = ribbon, aes(ymin = total5yLCI, ymax =total5yUCI, x = Time), fill = "blue", alpha = 0.2)+
  ggplot2::ggtitle("Gray Seal Bycatch")+
  ggplot2::ylab("Estimated Bycatch (n)")+
  ggplot2::scale_color_manual(name = element_blank(), values = c('red', 'black','blue'))+
  ggplot2::scale_linetype_manual(values=c(1,2,1), 
                                 labels = c("PBR", "Annual Estimates", "5yr rolling mean and CI"))+
  ggplot2::theme(#legend.position = "none",
    legend.title = element_blank(),
                 legend.position = c(0.2, 0.8), 
                 legend.text = element_text(size = 8), 
                 legend.background = element_rect(
                   colour = "transparent", fill = "transparent"))+
  guides(color = FALSE)+
  ecodata::theme_ts()+
  ecodata::theme_title()
