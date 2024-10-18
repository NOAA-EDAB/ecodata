
zoo_abund <- ecodata::zoo_abundance_anom %>% 
  #dplyr::mutate(Value = log10(Value+1)) %>% 
  dplyr::filter(EPU %in% c("GOM", "GB"),
         stringr::str_detect(Var, "Euphausiacea|Cnidaria")) %>% 
  dplyr::group_by(Var, EPU) %>% 
  dplyr::mutate(Value = as.numeric(Value), 
                hline = mean(Value)) 

gom_zoo<-zoo_abund %>% 
  dplyr::filter(EPU == "GOM") %>% 
  ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls(aes(x = Time, y = Value, group = Var)) +
  #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  ggplot2::geom_line(aes(x = Time, y = Value)) +
  ggplot2::geom_point(aes(x = Time, y = Value)) +
  ggplot2::ylab(expression("Anomaly")) +
  ggplot2::xlab(element_blank())+
  ggtitle("GOM Zooplankton Abundance Anomaly") +
  ggplot2::facet_wrap(Var~., ncol = 3) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0,
                                face = "italic"))+
  ecodata::theme_title()
  
gb_zoo<-zoo_abund %>% 
  dplyr::filter(EPU == "GB") %>% 
  ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls(aes(x = Time, y = Value, group = Var)) +
  #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  ggplot2::geom_line(aes(x = Time, y = Value)) +
  ggplot2::geom_point(aes(x = Time, y = Value)) +
  ggplot2::ylab(expression("Anomaly")) +
  ggplot2::xlab(element_blank())+
  ggtitle("GB Zooplankton Abundance Anomaly") +
  ggplot2::facet_wrap(Var~., ncol = 3) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0,
                                face = "italic"))+
  ecodata::theme_title()
 
cowplot::plot_grid(gb_zoo, gom_zoo, nrow = 2)
