
aqua <- ecodata::aquaculture %>%
  dplyr::filter(Region == c("ME", "MA", "NH", "RI")) %>% 
  dplyr::filter(!Value == "NA") %>% 
  dplyr::mutate(Time = as.integer(Time), 
                Value = as.numeric(Value))

ggplot2::ggplot() +
 #Highlight last ten years
  ggplot2::geom_line(data = aqua, aes(x = Time, y = Value, color = Region), size = lwd) +
  ggplot2::geom_point(data = aqua,aes(x = Time, y = Value, color = Region), size = pcex) +
  ggplot2::facet_wrap(~Var, scales = "free", nrow = 3)+
  ggplot2::ggtitle("Oyster Production in New England")+
  ggplot2::ylab(expression("Oysters production")) +
  ggplot2::xlab(element_blank())+
  theme(legend.position="bottom", 
        legend.title = element_blank())+
  scale_x_continuous(breaks=c(2009,2011,2013,2015, 2017, 2019))+
  ecodata::theme_ts()
