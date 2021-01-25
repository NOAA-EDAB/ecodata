
hp<- ecodata::aquaculture %>%
  dplyr::filter(Region == c("Maine", "Mass", "NEwHampshire", "RhodeLsland"))  %>% 
  dplyr::filter(!Value == "NA") %>% 
  dplyr::mutate(Time = as.integer(Time), 
                Value = as.numeric(Value))%>% 
  filter(Var == "Pieces") %>% 
  group_by(Time) %>% 
  summarise(Value = sum(Value)) %>% 
  ggplot2::ggplot() +
 #Highlight last ten years
  ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
  ggplot2::ggtitle("Total Oyster Production in New England")+
  ggplot2::ylab(expression("Oysters production (pieces)")) +
  ggplot2::xlab("")+
  theme(legend.position="bottom", 
        legend.title = element_blank())+
  scale_x_continuous(breaks=c(2009,2011,2013,2015, 2017, 2019))+
  ecodata::theme_ts()
