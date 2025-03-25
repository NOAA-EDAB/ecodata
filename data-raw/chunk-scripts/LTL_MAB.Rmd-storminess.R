
ecodata::storminess %>% 
  dplyr::filter(EPU == "MAB") %>%
  dplyr::mutate(Time = as.numeric(Year), 
                Value = as.numeric(Value)) %>% 
  group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value)) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value))+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::facet_wrap(~Var)+
  ggplot2::geom_hline(aes(yintercept = hline),
                      size = hline.size, 
                      alpha = hline.alpha,
                      linetype = hline.lty)+
  ecodata::geom_gls()+
  ggplot2::ylab("Number of Events") +
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()+
  ecodata::theme_facet()
