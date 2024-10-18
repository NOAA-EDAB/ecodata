
bt<- ecodata::bottom_temp_comp %>%
  dplyr::filter(EPU == "GOM") %>%
  dplyr::mutate(anom = Value - mean(Value), 
                hline = 0) %>% 
  ggplot2::ggplot()+ #plot
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = Time, y = anom)) +
  ecodata::geom_gls(aes(x = Time, y = anom)) +
  ggplot2::geom_point(aes(x = Time, y = anom)) +
  ggplot2::ylab("Temperature Anomaly (C)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ylim(-1.4,2)+
  ggplot2::ggtitle("Gulf of Maine") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
  ggplot2::theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))+
  ecodata::theme_title()


gb_bt<- ecodata::bottom_temp_comp %>%
  dplyr::filter(EPU == "GB") %>% 
  dplyr::mutate(anom = Value - mean(Value), 
                hline = 0) %>% 
  ggplot2::ggplot()+ #plot
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = Time, y = anom)) +
  ecodata::geom_gls(aes(x = Time, y = anom)) +
  ggplot2::geom_point(aes(x = Time, y = anom)) +
  ggplot2::ylab("Temperature Anomaly (C)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ylim(-1.4,2)+
  ggplot2::ggtitle("Georges Bank") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
  ggplot2::theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))+
  ecodata::theme_title()

plot_row<-cowplot::plot_grid(bt, gb_bt, ncol = 2)
title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Bottom Temperature Anomaly",
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0,1)
  )


cowplot::plot_grid(title, plot_row, ncol = 1, 
                   rel_heights = c(0.1, 1))
