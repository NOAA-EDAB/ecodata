
minlab <- seq(1987,2017,5)
maxlab <- seq(1989,2019,5)
minl<- as.numeric(sprintf('%02d',minlab %% 100))
maxl<- sprintf('%02d', maxlab %% 100)
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
  ggplot2::scale_x_continuous(breaks = minlab,labels = c("87-89", "92-94", 
                                                         "97-99", "02-04", 
                                                         "07-09", "12-14", "17-19")) +
   #ggplot2::scale_x_descrete(breaks = minl,labels = paste0(minl,"-",maxl),expand = c(0.01, 0.01)) +
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts()
