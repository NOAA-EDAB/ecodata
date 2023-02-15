
exp<- ecodata::exp_n %>% 
  tidyr::separate(Var, into = c("Var", "Season"), sep = "-") %>%
  filter( !EPU == "MAB", 
                                 !EPU == "SS", 
                                   Season == "SPRING", 
                                   stringr::str_detect(Var, 'AlbatrossSD|BigelowSD')) %>% 
    rename(VarSD = Var, 
         ValueSD = Value) 
exp2<- ecodata::exp_n %>% 
  tidyr::separate(Var, into = c("Var", "Season"), sep = "-") %>%
  filter(!EPU == "MAB", 
                                 !EPU == "SS", 
                                 Season == "SPRING", 
                                 Var %in% c("Albatross", "Bigelow"))   %>% 

  left_join(exp) %>% 
  mutate(upper = Value+ValueSD, 
         lower = Value - ValueSD)

gom<- exp2 %>% filter(EPU == "GOM") %>% 
ggplot2::ggplot(aes(x = Time, y = Value, fill = Var)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +

  ggplot2::geom_ribbon(aes(ymax = pmax(upper, 0), ymin = lower, x = Time),
              alpha = 0.5) +
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  #ggplot2::facet_wrap(EPU~.,scales = "free_y", ncol = 2) +
  ggplot2::ggtitle("GOM Expected Number of Species - Spring")+
  ecodata::geom_gls() +
  #ecodata::geom_lm()+
  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab(element_blank()) +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0), 
                 legend.title = element_blank(), 
                 axis.ticks.y = element_blank(), 
                 axis.text.y = element_blank())+
  ecodata::theme_title()

gb<- exp2 %>% filter(EPU == "GB") %>% 
ggplot2::ggplot(aes(x = Time, y = Value, fill = Var)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_ribbon(aes(ymax = pmax(upper, 0), ymin = lower, x = Time),
              alpha = 0.5) +
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  #ggplot2::facet_wrap(EPU~.,scales = "free_y", ncol = 2) +
  ggplot2::ggtitle("GB Expected Number of Species - Spring")+
  #ecodata::geom_lm() +
  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab("n species per 1000 ind") +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0), 
                 legend.title = element_blank(), 
                 legend.position = "none")+
  ecodata::theme_title()

gb+gom
