
ecodata::observed_sharks %>% 
  filter(!EPU == "MAB", 
         !EPU == "SS") %>% 
  ggplot()+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point(aes(x=Time, y = Value, color = Var))+
  ggplot2::geom_line(aes(x=Time, y = Value, color = Var))+
  ggplot2::facet_wrap(~EPU)+
  ggplot2::ggtitle("Observed Sharks")+
  ggplot2::ylab("Number of Sharks per Haul")+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()
