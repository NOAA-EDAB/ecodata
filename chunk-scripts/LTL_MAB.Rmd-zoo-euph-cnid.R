
zoo_abund <- ecodata::zoo_strat_abun %>% 
  dplyr::filter(EPU == "MAB") %>% 
         #stringr::str_detect(Var, "Euphausiacea|Cnidaria")) %>% 
  dplyr::mutate(Value = log10(Value+1)) %>% 
  dplyr::group_by(Var, EPU) %>% 
  dplyr::mutate(Value = as.numeric(Value), 
                hline = mean(Value, na.rm = TRUE)) 

zoo_abund %>% 
  ggplot2::ggplot(aes(x = Time, y = Value)) +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls() +
  #ecodata::geom_lm(aes(x = Time, y = Value))+
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::ylab(expression("Log Stratified Abundance")) +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Zooplankton Abundance") +
  ggplot2::facet_wrap(Var~., ncol = 3) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0,
                                face = "italic"))+
  ecodata::theme_title()
