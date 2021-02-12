
ecodata::forage_anomaly %>% 
  filter(!EPU == "MAB") %>% 
  pivot_wider(names_from = Var, values_from = Value) %>% 
  ggplot()+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point(aes(x=Time, y = Forage_Mean))+
  ggplot2::geom_line(aes(x=Time, y =Forage_Mean))+
  ggplot2::geom_ribbon(aes(ymin = Forage_Lower, ymax = Forage_Upper, x = Time), alpha = 0.3)+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed")+
  ggplot2::facet_wrap(~EPU)+
  ggplot2::ggtitle("Forage Anomalies")+
  ggplot2::ylab("Forage Anomaly")+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()
