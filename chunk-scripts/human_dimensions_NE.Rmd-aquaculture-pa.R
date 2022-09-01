
ecodata::aquaculture %>%
  ungroup() %>% 
  mutate(Region = as.character(Region)) %>% 
  dplyr::filter(!Region == "VA",
                !Region == "NJ",
                !Region == "MD",
                !Region == "NA",
                !Value == "NA") %>% 
  dplyr::mutate(Time = as.integer(Time), 
                Value = as.numeric(Value))%>% 
  filter(Var == "Production/Acre") %>% 
  group_by(Time) %>% 
  summarise(Value = sum(Value)) %>% 
  ggplot2::ggplot() +
 #Highlight last ten years
  ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
  ecodata::geom_lm(aes(x = Time, y = Value))+
  ggplot2::ggtitle("Total Oyster Production in New England")+
  ggplot2::ylab(expression("Production/Acre")) +
  ggplot2::xlab("")+
  theme(legend.position="bottom", 
        legend.title = element_blank())+
  scale_x_continuous(breaks=c(2009,2011,2013,2015, 2017, 2019))+
  ecodata::theme_ts()+
  ecodata::theme_title()
