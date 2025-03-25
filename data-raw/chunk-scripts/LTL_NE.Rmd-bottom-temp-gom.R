
gl_bt<- ecodata::bottom_temp_glorys%>% 
  dplyr::filter(EPU == "GOM")

bt<- ecodata::bottom_temp_insitu %>%
  dplyr::filter(EPU == "GOM",
         Var == "bottom temp anomaly in situ") %>%
  dplyr::mutate(hline = 0) 

gom_bottomtemp<- ggplot2::ggplot()+ #plot
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = bt$Time, y = bt$Value)) +
  ecodata::geom_gls(aes(x = bt$Time, y = bt$Value)) +
  ggplot2::geom_point(aes(x = bt$Time, y = bt$Value), size = 1) +
  ggplot2::geom_point(aes(x = gl_bt$Time, y = gl_bt$Value), size = 1, color = "red") +
  ggplot2::geom_line(aes(x = gl_bt$Time, y = gl_bt$Value), color = "red") +
  ggplot2::ylab("Temperature Anomaly (C)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ylim(-1.4,2)+
  ggplot2::ggtitle("Gulf of Maine") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::geom_hline(aes(yintercept = bt$hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
  ggplot2::theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))+
  ecodata::theme_title()

gb_gl_bt<- ecodata::bottom_temp_glorys%>% 
  dplyr::filter(EPU == "GB")

gb_bt<- ecodata::bottom_temp_insitu %>%
  dplyr::filter(EPU == "GB",
         Var == "bottom temp anomaly in situ") %>%
  dplyr::mutate(hline = 0) 

gb_bottomtemp<- ggplot2::ggplot()+ #plot
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = gb_bt$Time, y = gb_bt$Value)) +
  ecodata::geom_gls(aes(x = gb_bt$Time, y = gb_bt$Value)) +
  ggplot2::geom_point(aes(x = gb_bt$Time, y = gb_bt$Value), size = 1) +
  ggplot2::geom_point(aes(x = gb_gl_bt$Time, y = gb_gl_bt$Value), size = 1, color = "red") +
  ggplot2::geom_line(aes(x = gb_gl_bt$Time, y = gb_gl_bt$Value), color = "red") +
  ggplot2::ylab("Temperature Anomaly (C)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ylim(-1.4,2)+
  ggplot2::ggtitle("Georges Bank") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::geom_hline(aes(yintercept = gb_bt$hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
  ggplot2::theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))+
  ecodata::theme_title()

plot_row<-cowplot::plot_grid(gom_bottomtemp, gb_bottomtemp, ncol = 2)
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
