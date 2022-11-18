
zoo_div <- ecodata::zoo_diversity %>% 
  dplyr::filter(EPU %in% c("GOM","GB"))

gom_zoo_div <- zoo_div %>% 
  dplyr::filter(EPU == "GOM") %>% 
  tidyr::drop_na() %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, group = Var)) +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls() +
  #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  ggplot2::geom_line() +
  ggplot2::geom_point() +
#  ylim(1,2.8)+
  ggplot2::ylab("Shannon Diversity") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("GOM Zooplankton Diversity *No New Data") +
  ggplot2::facet_wrap(Var~., ncol = 3) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  ggplot2::geom_hline(aes(yintercept = mean(Value, na.rm = T)),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_blank())+
  ecodata::theme_title()

gom_zoo_div
