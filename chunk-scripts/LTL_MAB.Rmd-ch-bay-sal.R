
ches_sal<-ecodata::ch_bay_sal %>% 
  dplyr::filter(!Var == "UTCTime") %>% 
  tidyr::drop_na() %>%
  dplyr::mutate(Time =  as.numeric(str_sub(Time, 2, -1)),
         Time1 = as.Date(Time, origin = "2018-12-31")) %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value)

ches_sal %>% 
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(aes(x = Time1, ymin = AvgMinLim, ymax = AvgMaxLim))+
  ggplot2::geom_ribbon(aes(x = Time1, ymin = MinDataLim, ymax = MaxDataLim), alpha = 0.3)+
  ggplot2::geom_line(aes(x = Time1, y = Daily18), color = "blue") +
  ggplot2::geom_line(aes(x = Time1, y = Daily19), color = "red") +
  ggplot2::ylab(expression("PSU")) +
  ggplot2::ggtitle("Chesapeake Bay Salinity") +
  ecodata::theme_ts()
