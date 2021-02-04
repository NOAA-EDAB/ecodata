
ecodata::survey_shannon %>% filter(EPU == "MAB") %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, color = Var))+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ecodata::geom_gls() +
  ggtitle("MAB Survey Shannon Diversity Index")+
  ggplot2::ylab("Shannon")+
  ggplot2::scale_color_discrete(name = "Season", labels = c("Fall", "Spring"))+
  ecodata::theme_ts()
