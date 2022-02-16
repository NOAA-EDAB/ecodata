
ecodata::aquaculture %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(Region = as.character(Region)) %>% 
  dplyr::filter(!Region == "VA",
                !Region == "NJ",
                !Region == "MD",
                !Region == "NA",
                !Value == "NA") %>% 
  dplyr::mutate(Time = as.integer(Time), 
                Value = as.numeric(Value))%>% 
  dplyr::filter(Var == "Pieces") %>% 
  dplyr::group_by(Time) %>% 
  dplyr::summarise(Value = sum(Value)) %>% 
  dplyr::ungroup() %>% 
  ggplot2::ggplot() +
 #Highlight last ten years
  ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
  ggplot2::ggtitle("Total Oyster Production in New England")+
  ggplot2::ylab(expression("Oysters production (Pieces)")) +
  ggplot2::xlab("")+
  ggplot2::theme(legend.position="bottom", 
        legend.title = element_blank())+
  ggplot2::scale_x_continuous(breaks=c(2009,2011,2013,2015, 2017, 2019))+
  ecodata::theme_ts()+
  ecodata::theme_title()
