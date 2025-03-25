
ecodata::bottom_temp_glorys %>%
  dplyr::filter(EPU == epu_abbr)%>%
  dplyr::group_by(EPU) %>%
  dplyr::mutate(hline = mean(Value)) %>%
ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = Time, y = Value)) +
  #ecodata::geom_gls(aes(x = Time, y = Value)) +
  ggplot2::geom_point(aes(x = Time, y = Value), size = 1) +
  ggplot2::ylab("Temperature (C)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Bottom temperature GLORYS anomaly") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))+
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts()
