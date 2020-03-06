
minlab <- seq(1985,2015,5)
maxlab <- seq(1987,2017,5)



ecodata::ches_bay_wq %>% 
  dplyr::mutate(hline = mean(Value)) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value)) +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ecodata::geom_gls() +
  ggplot2::ylab(expression("Estimated attainment, percent")) +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Chesapeake Bay Water Quality Attainment") +
  ggplot2::scale_x_continuous(breaks = minlab,labels = paste0(minlab,"-",maxlab),expand = c(0.01, 0.01)) +
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts()
