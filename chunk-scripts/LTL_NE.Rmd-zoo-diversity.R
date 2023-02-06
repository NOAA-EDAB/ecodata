
zoo_div <- ecodata::zoo_diversity %>% 
  dplyr::filter(EPU %in% c("GOM","GB"), 
                Time > 1991)

gb_zoo_div <- zoo_div %>% 
  dplyr::filter(EPU == "GB") %>% 
  #dplyr::mutate(hline = mean(Value)) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, group = Var)) +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls() +
  #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  ggplot2::geom_line() +
  ggplot2::geom_point() +
#  ggplot2::ylim(1,2.8)+
  ggplot2::ylab("Shannon Diversity") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("GB Zooplankton Diversity") +
  ggplot2::facet_wrap(Var~., ncol = 3) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  ggplot2::geom_hline(aes(yintercept = mean(Value, na.rm = TRUE)),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_blank())+
  ecodata::theme_title()

gom_zoo_div <- zoo_div %>% 
  dplyr::filter(EPU == "GOM") %>% 
  dplyr::mutate(hline = mean(Value, rm.na = TRUE)) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, group = Var)) +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls() +
  #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  ggplot2::geom_line() +
  ggplot2::geom_point() +
#  ggplot2::ylim(1,2.8)+
  ggplot2::ylab("Shannon Diversity") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("GOM Zooplankton Diversity") +
  ggplot2::facet_wrap(Var~., ncol = 3) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  ggplot2::geom_hline(aes(yintercept = mean(Value, rm.na = TRUE)),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_blank())+
  ecodata::theme_title()

gb_zoo_div + gom_zoo_div + patchwork::plot_layout(ncol = 2)
