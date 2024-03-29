
ecodata::trans_dates %>% 
  dplyr::filter(EPU %in% c("GB","GOM"), 
                Var == "sumlen", 
                !Value == "NA") %>% 
  ggplot(aes(x= Time, y = Value))+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_point()+
  geom_line()+
  ecodata::geom_gls() +
  ggplot2::theme(strip.text=element_text(hjust=0),
                 plot.title = element_text(size = 12))+
  ecodata::theme_title()+
  ylab("Number of Days")+
  xlab(element_blank())+
  ecodata::theme_ts()+
  ggplot2::facet_wrap(.~EPU)+
  ecodata::theme_facet()
