
knitr::include_graphics(file.path(image.dir, "hms-too-big.png"))

ecodata::hms_cpue %>%
  ggplot()+
 ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point(aes(x=Time, y = Value))+
  ggplot2::geom_line(aes(x=Time, y = Value))+
  ggplot2::facet_wrap(~Var, scales = "free")+
  ggplot2::ggtitle("HMS POP CPUE ALL")+
  ggplot2::ylab("Number per Haul")+
  ecodata::theme_ts()
