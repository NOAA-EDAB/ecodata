
gl_bt<- ecodata::bottom_temp_glorys%>% 
  dplyr::filter(EPU == "GOM")

bt<- ecodata::bottom_temp %>%
  dplyr::filter(EPU == "GOM",
         Var == "bottom temp anomaly in situ") %>%
  dplyr::mutate(hline = 0) 

ggplot2::ggplot()+ #plot
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
  ggplot2::ggtitle("GOM Bottom Temperature Anomaly") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::geom_hline(aes(yintercept = bt$hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
  ggplot2::theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))+
  ecodata::theme_title()
