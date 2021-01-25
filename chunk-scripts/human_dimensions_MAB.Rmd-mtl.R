
ecodata::ppr %>% 
  dplyr::filter(Var == "MTL") %>% 
  dplyr::group_by(EPU) %>% 
  dplyr::mutate(hline = mean(Value)) %>% 
  dplyr::filter(EPU == "MAB") %>% 
  ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point(aes(x = Time, y = Value))+
  ggplot2::geom_line(aes(x = Time, y = Value))+
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  #ggplot2::facet_wrap( ~ EPU)+
  ggplot2::ggtitle("Mean Trophic Level")+
  ggplot2::ylab("")+
  ecodata::theme_ts()
