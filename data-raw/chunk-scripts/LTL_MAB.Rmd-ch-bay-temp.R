
ecodata::ch_bay_temp %>%
  tidyr::pivot_wider(names_from = Var, values_from = Value) %>% 
  dplyr::mutate(YearLTAC = (YearLTA - 32)*(5/9),
                minLTAC = (minLTA - 32)*(5/9),
                maxLTAC = (maxLTA - 32)*(5/9), 
                YearC = (Year - 32)*(5/9)) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(aes(x = Time, ymin = minLTAC, ymax = maxLTAC), fill = "grey", alpha = 0.5)+
  
  ggplot2::geom_line(aes(x = Time, y = YearLTAC, color= "Long Term Average 2010-2020")) +
  ggplot2::geom_line(aes(x = Time, y = YearC, color = "Daily 2022")) +
  ggplot2::ylab("Temperature (C)") +
  ggplot2::ggtitle("Chesapeake Bay Temperature") +
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
