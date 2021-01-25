
ecodata::ppr %>% 
  dplyr::filter(Var == "Fogarty") %>% 
  dplyr::mutate(Value = Value*1000) %>% 
  dplyr::group_by(EPU) %>% 
  dplyr::mutate(hline = mean(Value)) %>% 
  dplyr::filter(EPU == "MAB") %>% 
  ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_hline(yintercept = .92, color = "green", linetype = "dashed")+
  ggplot2::geom_hline(yintercept = .22, color = "green", linetype = "dashed")+
  ggplot2::geom_hline(yintercept = 1, color = "red", linetype = "dashed")+
  ggplot2::geom_hline(yintercept = 2.5, color = "red", linetype = "dashed")+
  ggplot2::geom_point(aes(x = Time, y = Value))+
  ggplot2::geom_line(aes(x = Time, y = Value))+
  #ggplot2::geom_line(data = a, aes(x = Time, y = Value), linetype = "dashed")+
  #ggplot2::geom_hline(aes(yintercept = hline),
  #         size = hline.size,
  #         alpha = hline.alpha,
  #         linetype = hline.lty)+
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  #ggplot2::facet_wrap( ~ EPU)+
  ggplot2::ggtitle("Fogarty Index")+
  ggplot2::ylab("PPT")+
  ecodata::theme_ts()
