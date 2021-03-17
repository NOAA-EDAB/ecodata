
ecodata::seal_pups %>% 
  ggplot()+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point(aes(x=Time, y = Count))+
  ggplot2::geom_line(aes(x=Time, y = fitted, line.type = "dashed"))+
  ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper, x = Time), alpha = 0.2)+
  #ggplot2::geom_ribbon(aes(x=Time, y = upper, line.type = "dashed"))+
  #ggplot2::geom_line(aes(x=Time, y = lower, line.type = "dashed"))+
  ggplot2::facet_wrap(~Colony, scales = "free")+
  ggplot2::ggtitle("Estimated Gray Seal Pup Births")+
  ggplot2::ylab("Pup Count")+
  ggplot2::xlab(element_blank())+
  #ggplot2::scale_color_discrete(name = "Category")+
  ecodata::theme_ts()+
  ecodata::theme_title()+
  ecodata::theme_facet()
