
cp<- ecodata::cold_pool %>% 
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Val, na.rm = T)) %>% 
  ggplot2::ggplot() + 
 #Highlight last ten years
  #ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #    xmin = x.shade.min , xmax = x.shade.max) +
  ecodata::geom_gls(aes(x = Time, y = Val),
             alpha = trend.alpha, size = trend.size) +
  ggplot2::geom_line(aes(x = Time, y = Val), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Val), size = pcex) +
  ggplot2::geom_hline(aes(yintercept = hline),
     size = hline.size,
     alpha = hline.alpha,
     linetype = hline.lty)+
  ggplot2::ggtitle("Cold Pool Index")+
  ggplot2::facet_wrap(~Var, scales = "free")+
  ggplot2::ylab("Cold Pool") +
  ggplot2::xlab("")+
  ecodata::theme_ts()

plotly::ggplotly(cp) 
