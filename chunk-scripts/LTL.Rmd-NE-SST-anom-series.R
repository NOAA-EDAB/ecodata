
ne_anom <- ecodata::seasonal_oisst_anom %>% 
  dplyr::filter(EPU %in% c("GB","GOM")) %>% 
  dplyr::mutate(hline = 0,
           Var = stringr::str_to_title(stringr::str_extract(Var,"winter|spring|summer|fall")))
ne_anom$Var <- factor(ne_anom$Var, levels= c("Winter","Spring","Summer","Fall"))

ne_anom_plt <- ggplot2::ggplot(data = ne_anom, 
       aes(x = Time, y = Value, color = EPU, group = EPU))+
     ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line()+
  ggplot2::geom_point()+
  ggplot2::ylim(-2,3)+
  ecodata::geom_gls() +
  ggplot2::ylab(expression("SST Anomaly (C)")) +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Gulf of Maine & Georges Bank SST Anomaly") +
  ggplot2::scale_color_manual(values = c("black","indianred"))+
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ggplot2::facet_wrap(Var ~., ncol = 2, scales = "free_y")+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))
ne_anom_plt
