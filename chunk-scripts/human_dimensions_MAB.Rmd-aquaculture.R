
aqua <- ecodata::aquaculture %>%
  dplyr::filter(Region %in% c("MD", "VA", "NJ")) %>% 
  dplyr::filter(!Value == "NA") %>% 
  dplyr::mutate(Time = as.integer(Time), 
                Value = as.numeric(Value))

ggplot2::ggplot() +
 #Highlight last ten years
  ggplot2::geom_line(data = aqua, aes(x = Time, y = Value, color = Region), size = lwd) +
  ggplot2::geom_point(data = aqua,aes(x = Time, y = Value, color = Region), size = pcex) +
  #ggplot2::facet_wrap(~Region, nrow = 3)+
  ggplot2::ggtitle("Oyster Production in MAB")+
  ggplot2::ylab(expression("Oysters production")) +
  ggplot2::xlab("")+
  scale_x_continuous(breaks=c(2009,2011,2013,2015, 2017, 2019))+
  ecodata::theme_ts()

# aqua %>% group_by(Time) %>% summarise(Value = sum(Value)) %>% 
#   dplyr::filter(Time %in% c(max(Time), max(Time-1))) %>% 
#   dplyr::summarise(m = mean(Value))
# 
# aqua %>% group_by(Time) %>% summarise(Value = sum(Value)) %>% 
#   dplyr::filter(Time %in% c(max(Time-2), max(Time-3),  max(Time-4)), 
#                 !Value == "NA") %>%  
#   dplyr::summarise(m= mean(Value))
