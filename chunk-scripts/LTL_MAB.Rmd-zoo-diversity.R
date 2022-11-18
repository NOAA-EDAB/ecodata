
zoo_div <- ecodata::zoo_diversity %>% 
  dplyr::filter(EPU == epu_abbr)

zoo_div %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, group = Var)) +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls() +
  #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::ylab("Shannon Diversity") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Zooplankton Diversity") +
  ggplot2::facet_wrap(Var~., ncol = 3) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  ggplot2::geom_hline(aes(yintercept = mean(Value)),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_blank())+
  ecodata::theme_title()
