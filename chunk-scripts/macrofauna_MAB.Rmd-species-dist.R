
spec_dist <- ecodata::species_dist %>% 
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value))

asd <- spec_dist %>% 
  dplyr::filter(Var == "along-shelf distance", 
                !Time == 2020) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value,
               group = Var)) + 
 #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls() +
  ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::ggtitle("Along-shelf distance")+
  ggplot2::ylab(expression("Distance (km)")) +
  ggplot2::xlab("")+
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts()+
  ecodata::theme_title() 

depth <- spec_dist %>% 
  dplyr::filter(Var == "depth", 
                !Time == 2020) %>% 
  dplyr::mutate(Value = Value, 
         hline = mean(Value)) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value,
               group = Var)) + 
 #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls() +
  ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::scale_y_continuous(trans = "reverse")+
  ggplot2::ggtitle("Depth") +
  ggplot2::ylab(expression("Depth (m)")) +
  ggplot2::xlab("")+
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
  ecodata::theme_title()


asd + depth + patchwork::plot_layout(ncol = 1) 
