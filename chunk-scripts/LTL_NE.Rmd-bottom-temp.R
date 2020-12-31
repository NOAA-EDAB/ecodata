
bot_temp_insitu_gom <- ecodata::bottom_temp %>%
  dplyr::filter(EPU == "GOM",
         Var == "bottom temp anomaly in situ") %>% 
  dplyr::mutate(hline = 0) %>% 
  ggplot2::ggplot()+ #plot
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = Time, y = Value)) +
  ecodata::geom_gls(aes(x = Time, y = Value)) +
  ggplot2::geom_point(aes(x = Time, y = Value), size = 1) +
  ggplot2::ylab("Temperature Anomaly (C)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("GOM Bottom Temperature Anomaly") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
  ggplot2::theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))


bot_temp_insitu_gb <- ecodata::bottom_temp %>%
  dplyr::filter(EPU == "GB",
         Var == "bottom temp anomaly in situ") %>%
  dplyr::mutate(hline = 0) %>% 
  ggplot2::ggplot()+ #plot
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = Time, y = Value)) +
  ecodata::geom_gls(aes(x = Time, y = Value)) +
  ggplot2::geom_point(aes(x = Time, y = Value), size = 1) +
  ggplot2::ylab("Temperature Anomaly (C)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("GB Bottom Temperature Anomaly") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
  ggplot2::theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))


# bot_temp_insitu_gb +
# bot_temp_insitu_gom +
#   patchwork::plot_layout(ncol =  1) &
#   ggplot2::theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
