
exp<- ecodata::exp_n %>% 
  tidyr::separate(Var, into = c("Var", "Season"), sep = "-") %>% 
  dplyr::filter( EPU == "MAB", Season == "FALL", 
                 stringr::str_detect(Var, 'AlbatrossSD|BigelowSD')) %>% 
  dplyr::rename(VarSD = Var, ValueSD = Value) 

exp2<- ecodata::exp_n %>% 
  tidyr::separate(Var, into = c("Var", "Season"), sep = "-") %>% 
  dplyr::filter(EPU == "MAB", Season == "FALL",
         Var %in% c("Albatross", "Bigelow"))   %>% 
  dplyr::left_join(exp) %>% 
  dplyr::mutate(upper = Value+ValueSD, 
         lower = Value - ValueSD)

exp2 %>% 
ggplot2::ggplot(aes(x = Time, y = Value, fill = Var)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +

  ggplot2::geom_ribbon(data = exp2, aes(ymax = pmax(upper, 0), ymin = lower, x = Time),
              alpha = 0.5) +
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  #ecodata::geom_lm()+
  # scale_color_manual(values = series.col, aesthetics = "color")+
  #ggplot2::guides(color = FALSE) +
  #ggplot2::geom_hline(aes(yintercept = hline,
  #               group = Var),
  #           size = hline.size,
  #           alpha = hline.alpha,
  #           linetype = hline.lty)+
  ggplot2::facet_wrap(EPU~.,scales = "free_y", ncol = 2) +
  ggplot2::ggtitle("Expected Number of Species - Fall")+
  #ecodata::geom_gls() +
  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab("n species per 1000 ind") +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0), 
                 legend.title = element_blank())+
  ecodata::theme_title()
