
ecodata::ppr %>% 
  dplyr::filter(Var == "Ryther") %>% 
  dplyr::group_by(EPU) %>% 
  dplyr::mutate(hline = mean(Value)) %>% 
  dplyr::filter(EPU == "MAB") %>% 
  ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_hline(yintercept = 0.3, color = "green", linetype = "dashed")+
  ggplot2::geom_hline(yintercept = 1.1, color = "green", linetype = "dashed")+
  ggplot2::geom_hline(yintercept = 3, color = "red", linetype = "dashed")+
  ggplot2::geom_hline(yintercept = 5, color = "red", linetype = "dashed")+
  ggplot2::geom_point(aes(x = Time, y = Value))+
  ggplot2::geom_line(aes(x = Time, y = Value))+
  #ggplot2::geom_line(data = a, aes(x = Time, y = Value), linetype = "dashed")+
  #ggplot2::geom_hline(aes(yintercept = hline),
  #         size = hline.size,
  #         alpha = hline.alpha,
  #         linetype = hline.lty)+
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  #ggplot2::facet_wrap( ~ EPU)+
  ggplot2::ggtitle("Ryther Index")+
  ggplot2::ylab("mt km-2 y-1")+
  ecodata::theme_ts()
