
a<- ecodata::ppr %>% 
  dplyr::group_by(EPU) %>% 
  dplyr::mutate(hline = mean(Value)) %>% 
  dplyr::filter(Var == "PPR",
                EPU == "MAB", 
                Time <= 1997) 

ecodata::ppr %>% 
  dplyr::group_by(EPU) %>% 
  dplyr::mutate(hline = mean(Value)) %>% 
  dplyr::filter(Var == "PPR",
                EPU == "MAB", 
                Time >= 1997) %>% 
  ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point(aes(x = Time, y = Value))+
  ggplot2::geom_line(aes(x = Time, y = Value))+
  ggplot2::geom_line(data = a, aes(x = Time, y = Value), linetype = "dashed")+
  # ecodata::geom_lm(aes(x = Time, y = Value,
  #              group = Var))+
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::ggtitle("Primary Production Required")+
  ggplot2::ylab("Proportion of Total PPD ")+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
