
sp_cat<- ecodata::hms_category
ecodata::hms_cpue %>% 
  filter(stringr::str_detect(Var, "SHARK")) %>% 
  rename(COMMON_POP = Var) %>% 
  left_join(sp_cat) %>% 
  group_by(Time, SP_CATEGORY) %>% 
  summarise(Value = sum(Value)) %>% 
  rename("Var" = "SP_CATEGORY") %>% 
  filter(!Var == "NA") %>% 
  ggplot()+
 ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point(aes(x=Time, y = Value, color = Var))+
  ggplot2::geom_line(aes(x=Time, y = Value, color = Var))+
  ggplot2::scale_color_discrete(name = "Category")+
  #ggplot2::facet_wrap(~Var, scales = "free")+
  ggplot2::ggtitle("HMS POP SHARK CPUE")+
  ggplot2::ylab("Number per Haul")+
  ecodata::theme_ts()+
  ecodata::theme_title()
