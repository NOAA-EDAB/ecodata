
 ecodata::wind_port %>% filter(EPU == "MAB") %>%
  dplyr::arrange(desc(port_total)) %>%
  dplyr::mutate(City = factor(City, levels = unique(City))) %>% 
  ggplot2::ggplot(aes(fill = Var, y = reorder(City, port_total), x = Value))+
  ggplot2::geom_bar( stat="identity" )+
  ggplot2::theme(legend.position = "bottom")+
  ggplot2::ggtitle("Port Revenue from WEA")+
  ggplot2::xlab("Percent Port Revenue")+
  ggplot2::ylab("Port")+
  scale_fill_discrete(name = "", labels = c("% revenue remaining", "% revenue from WEA"))+
  ecodata::theme_ts()
