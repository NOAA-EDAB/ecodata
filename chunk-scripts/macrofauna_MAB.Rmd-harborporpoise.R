
ecodata::harborporpoise %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x=YEAR, y = EST), color = "black")+
  ggplot2::geom_line(aes(x=YEAR, y = EST), color = "black")+
  ggplot2::geom_point(aes(x=YEAR, y = PBR), color = "red")+
  ggplot2::geom_line(aes(x=YEAR, y = PBR), color = "red")+
  ggplot2::geom_ribbon(aes(ymin = LCI, ymax =UCI, x = YEAR), alpha = 0.3)+
  ggplot2::ggtitle("Harbor Porpoise Byctach")+
  ggplot2::ylab("Estimated Bycatch (n)")+
  ecodata::theme_ts()
  
# hp %>% 
#   dplyr::filter(YEAR %in% c(max(YEAR), max(YEAR-1))) %>% 
#   dplyr::summarise(m = mean(EST))
# 
# hp %>% 
#   dplyr::filter(YEAR %in% c(max(YEAR-2), max(YEAR-3),  max(YEAR-4))) %>% 
#   dplyr::summarise(m= mean(EST))
