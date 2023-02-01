
ecodata::trans_dates %>% 
  dplyr::filter(EPU == "MAB", 
                Var %in% c("sumlen30"), 
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
  ylab("# of Days")+
  xlab(element_blank())+
  ggplot2::ggtitle("Number of Days betwen Spring and Fall Transition Dates")
  ecodata::theme_ts()
