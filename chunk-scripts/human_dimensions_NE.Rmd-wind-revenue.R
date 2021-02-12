
ecodata::wind_revenue %>% 
  dplyr::mutate(Value = Value/1000000, 
                Time = as.numeric(Time)) %>% 
  tidyr::separate(Var, into=c("Var", "Trash"), sep = "[(]") %>% 
  dplyr::select(!Trash) %>% 
  #dplyr::group_by(EPU) %>% 
 # dplyr::mutate(hline = mean(Value)) %>% 
  dplyr::filter(EPU == "NE") %>% 
  ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point(aes(x = Time, y = Value, color = Var))+
  ggplot2::geom_line(aes(x = Time, y = Value, color = Var))+
  #ggplot2::facet_wrap(~Var, ncol = 1, scales = "free")+
  #ggplot2::geom_hline(aes(yintercept = hline),
  #         size = hline.size,
  #         alpha = hline.alpha,
  #         linetype = hline.lty)+
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::ggtitle("Fishery Revenue in Wind Lease Areas")+
  ggplot2::ylab("2019 Constant Million Dollars ")+
  ggplot2::xlab(element_blank())+
  ggplot2::theme(legend.title = element_blank())+
  ecodata::theme_ts()
