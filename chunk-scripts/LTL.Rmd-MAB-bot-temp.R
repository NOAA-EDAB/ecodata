
temp_anom <- ecodata::bottom_temp %>% 
  dplyr::filter(EPU == epu_abbr) %>% 
  tidyr::complete(Time = tidyr::full_seq(min(bottom_temp$Time):max(bottom_temp$Time),1),
           tidyr::nesting(Var)) %>% 
  dplyr::mutate(hline = 0)

temp_anom %>%
 dplyr::filter(Var == "bottom temp anomaly in situ") %>%
ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = Time, y = Value)) +
  ecodata::geom_gls(aes(x = Time, y = Value)) +
  ggplot2::geom_point(aes(x = Time, y = Value), size = 1) +
  ggplot2::ylab("Temperature (C)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Bottom temperature anomaly") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))+
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts()
