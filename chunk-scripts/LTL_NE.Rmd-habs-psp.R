
ecodata::habs %>% 
  filter(Source == "PSP") %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, color = Var))+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ecodata::theme_ts()+
  ggplot2::theme(strip.text=element_text(hjust=0,
                                face = "italic"), 
        axis.title.y = element_text(angle = 90)) +
  ggplot2::scale_color_discrete(name = "State")+
  ecodata::theme_title()+
  ggplot2::ylab(expression("Percent PSP"))+
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Percent PSP Samples in Blue Mussels")+
  ecodata::theme_facet()
