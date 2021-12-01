
 ecodata::wind_dev_speed %>% mutate(Value = as.numeric(Value)/1000000, 
                                                Time = as.integer(Time)) %>% 
  dplyr::filter(Var == "Tot_Area_Acres") %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x = Time, y = Value, color = Report_year))+
  ggplot2::geom_line(aes(x = Time, y = Value, color = Report_year))+
  #ggplot2::geom_point(aes(x = Time, y = `2022`))+
  #ggplot2::geom_line(aes(x = Time, y = `2022`))+
  ggplot2::ylab("Total Area (Million Acres)")+
  ggplot2::xlab("Project Construction Year")+
  ggplot2::ggtitle("Wind Lease Cumulative Area")+
  ecodata::theme_ts()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks=c(2020,2022, 2024,2026, 2028, 2030))+
  ecodata::theme_title()+
  scale_colour_discrete(name="Year Reported", labels=c("2021","2022"))+
  theme(legend.position = c(0.8, 0.2))
