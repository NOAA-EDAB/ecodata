
total_area<- ecodata::wind_dev_speed %>% mutate(Value = as.numeric(Value)/1000000, 
                                                Time = as.integer(Time)) %>% 

  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x = Time, y = Value))+
  ggplot2::geom_line(aes(x = Time, y = Value, group=1))+
  ggplot2::ylab("Total Area (Million Acres)")+
  ggplot2::xlab("Project End Year")+
  ggplot2::ggtitle("Cumulative Area")+
  ecodata::theme_ts()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks=c(2020,2022, 2024,2026, 2028))

total_area
