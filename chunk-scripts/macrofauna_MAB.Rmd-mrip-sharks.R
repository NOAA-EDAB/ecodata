
ecodata::rec_hms %>% 
  filter(EPU == "MAB") %>% 
  #mutate(Value = Value/1000) %>% 
  ggplot()+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point(aes(x=Time, y = Value, color = Var))+
  ggplot2::geom_line(aes(x=Time, y = Value, color = Var))+
  ggplot2::ggtitle("Rec Shark - Total Catch")+
  ggplot2::ylab("Number of Fish")+
  ggplot2::xlab(element_blank())+
  ggplot2::scale_color_discrete(name = "Category")+
  ecodata::theme_ts()+
  ecodata::theme_title()
