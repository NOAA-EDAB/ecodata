
bird<- ecodata::seabird_mab %>% 
  dplyr::mutate(hline = mean(Value))

bird %>% 
  ggplot2::ggplot() +
#Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = Time, y = Value, color = Group), size = lwd-0.75) +
  ggplot2::geom_point(aes(x = Time, y = Value, color = Group), size = pcex-0.75) +
  #geom_gls(aes(x = Time, y = Value)) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01),limits = c(1991,2018)) +
  ggplot2::ggtitle("Seabird Abundance") +
  ggplot2::ylab(expression("Number of Breeding Pairs")) +
  ggplot2::xlab("")+
  ggplot2::geom_hline(aes(yintercept = hline),
           color = "black",
          size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ggplot2::theme(legend.position="bottom", legend.direction = "horizontal", 
        legend.title = element_blank(), legend.margin=margin(t = -20)) +
  ecodata::theme_ts()
