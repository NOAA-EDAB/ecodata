
ecodata::survey_shannon %>% filter(!EPU == "MAB", 
                                   !EPU == "SS") %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, color = Var))+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ecodata::geom_gls() +
  ggtitle("Survey Shannon Diversity Index")+
  ggplot2::facet_wrap(~EPU)+
  ggplot2::ylab("Shannon")+
  ggplot2::xlab(element_blank())+
  ggplot2::scale_color_discrete(name = "Season", labels = c("Fall", "Spring"))+
  ecodata::theme_ts()+
  ecodata::theme_title()
