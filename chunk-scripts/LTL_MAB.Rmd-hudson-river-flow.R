
ecodata::hudson_river_flow %>% 
  ggplot2::ggplot(aes(x = Time, y = Value))+
    ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ecodata::geom_gls(aes(x = Time, y = Value)) +
  #ecodata::geom_lm(aes(x = Time, y = Value))+
  ecodata::theme_ts()+
  ecodata::theme_title()+
  ggplot2::ylab(expression("mean flowrate (m"^3*" s"^-1*")"))+
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Hudson River Flow")
