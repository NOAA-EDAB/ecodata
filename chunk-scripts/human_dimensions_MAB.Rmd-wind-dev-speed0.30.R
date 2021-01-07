
labels<- ecodata::wind_dev_speed %>% 
  group_by(Time) %>% 
  summarise("Projects(n)" = length(Project_Name)) %>% 
  filter(!Time == "NA")

total_area<- ecodata::wind_dev_speed %>% 
  filter(Var == "Cumulative_area",
         !Time == "NA") %>% 
  group_by(Time) %>% 
  summarise(Value_2 = sum(Value)) %>% 
  mutate(Value_3 = cumsum(Value_2)/1000000) %>% 

  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x = Time, y = Value_3))+
  ggplot2::geom_line(aes(x = as.factor(Time), y = Value_3, group=1))+
  ggplot2::ylab("Total Area (Million Acres)")+
  ggplot2::xlab("")+
  ggplot2::ggtitle("Cumulative Area")+
  ecodata::theme_ts()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
dist_area<- ecodata::wind_dev_speed%>% 
  filter(Var == "Cumulative_seafloor_disturbance",
         !Time == "NA") %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x = Time, y = Value))+
  ggplot2::ylab(" Total Seafloor Distubrance (Acres)")+
  ggplot2::xlab("")+
  ggplot2::ggtitle("Cumulative Seafloor Disturbance")+
  theme(axis.text.x = element_text(angle = 45))

foundations<- ecodata::wind_dev_speed%>% 
  filter(Var == "Cumulative_foundations",
         !Time == "NA") %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x = Time, y = Value))+
  ggplot2::ylab(" Foundations (n)")+
  ggplot2::xlab("")+
  ggplot2::ggtitle("Cumulative Number of Foundations")+
  theme(axis.text.x = element_text(angle = 45))

# gridExtra::grid.arrange(
#   gridExtra::tableGrob(labels, theme = gridExtra::ttheme_minimal(base_size = 8)),
#   total_area,
#   ncol = 2,
#   widths = c(0.5, 1.5),
#   clip = FALSE
# )

total_area
