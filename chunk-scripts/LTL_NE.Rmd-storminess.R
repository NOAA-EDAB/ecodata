
storm<- ecodata::storminess %>% 
  dplyr::filter(EPU %in% c("GB", "GOM")) %>%
  dplyr::mutate(Time = as.numeric(Year), 
                Value = as.numeric(Value)) %>% 
  group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value))

#storm$Var <- factor(storm$Var, 
#                       levels = c("Georges Bank", 
#                                  "Western Gulf of Maine",
#                                  "Eastern Gulf of Maine"))
storm %>% 
  ggplot2::ggplot(aes(x = Time, y = Value))+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::facet_wrap(~Var, ncol = 2)+
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ecodata::geom_gls()+
  ggplot2::ylab("Number of Events") +
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()+
  ecodata::theme_facet()
