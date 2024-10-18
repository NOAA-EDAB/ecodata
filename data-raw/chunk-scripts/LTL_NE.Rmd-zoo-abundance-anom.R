
d<- ecodata::zoo_regime %>% 
  dplyr::filter(Var %in% c('pseudo_100m3', 'ctyp_100m3', 'calfin_100m3'), 
                EPU %in% c('GOM', 'GB')) %>% 
  dplyr::mutate(Var = dplyr::recode(Var,  "ctyp_100m3" = "C.typicus", 
                                    "calfin_100m3" = "C.finmachicus")) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, color = Var)) +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::facet_wrap(~EPU, ncol = 2, scales = "free")+
  #ecodata::geom_gls()+
  ggplot2::ylab("Abundance Anomaly") +
  ggplot2::geom_hline(aes(yintercept = 0),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ggplot2::xlab(element_blank())+
  scale_color_discrete(
    labels = c(expression(italic("C.finmarchicus")), 
              expression(italic("C.typicus")), 
              expression(italic("Pseudocalanus spp."))))+
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()+
  ecodata::theme_facet()
d
