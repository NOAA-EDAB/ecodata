
ecodata::cold_pool %>% 
    dplyr::mutate(hline = mean(Value, na.rm = T)) %>% 
  ggplot2::ggplot() + 
 #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -3, ymax = 3) +
  ecodata::geom_gls(aes(x = Time, y = Value),
             alpha = trend.alpha, size = trend.size) +
  ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
  ggplot2::geom_hline(aes(yintercept = hline),
     size = hline.size,
     alpha = hline.alpha,
     linetype = hline.lty)+
  ggplot2::ggtitle("Cold Pool Index")+
  ggplot2::ylab(expression("Cold Pool Temp Anomaly (C) ")) +
  ggplot2::xlab("")+
  ecodata::theme_ts()
