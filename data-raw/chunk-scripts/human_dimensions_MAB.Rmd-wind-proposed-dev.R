
 ecodata::wind_dev_speed %>% mutate(Value = as.numeric(Value)/1000000, 
                                                Time = as.integer(Time)) %>% 
  dplyr::filter(Var == "Acres", 
                Report_year == "year2023") %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x = Time, y = Value))+
  ggplot2::geom_line(aes(x = Time, y = Value))+
  #ggplot2::geom_point(aes(x = Time, y = `2022`))+
  #ggplot2::geom_line(aes(x = Time, y = `2022`))+
  ggplot2::ylab("Total Area (Million Acres)")+
  ggplot2::xlab("Project Construction Year")+
  ggplot2::ggtitle("Proposed Wind Development")+
  ecodata::theme_ts()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks=c(2020,2022, 2024,2026, 2028, 2030))+
  ecodata::theme_title()+
  scale_colour_discrete(name="Year Reported", labels=c("2021","2022"))
