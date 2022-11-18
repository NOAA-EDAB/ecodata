
ecodata::ppr %>% 
  dplyr::filter(Var == "Ryther") %>% 
  dplyr::group_by(EPU) %>% 
  dplyr::mutate(hline = mean(Value), 
                gr_up = c(1.1),
                rd_up = c(5),
                gr_lw = c(0.3),
                rd_lw = c(3)) %>% 
  dplyr::filter(EPU != "MAB") %>% 
  ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_ribbon( aes(ymin = gr_lw, ymax = gr_up, x = Time), fill = "darkolivegreen3", alpha = 0.5)+
  ggplot2::geom_ribbon( aes(ymin = rd_lw, ymax = rd_up, x = Time), fill = "sandybrown", aplha = 0.5)+
  # ggplot2::geom_hline(yintercept = 0.3, color = "green", linetype = "dashed")+
  # ggplot2::geom_hline(yintercept = 1.1, color = "green", linetype = "dashed")+
  # ggplot2::geom_hline(yintercept = 3, color = "red", linetype = "dashed")+
  # ggplot2::geom_hline(yintercept = 5, color = "red", linetype = "dashed")+
  ggplot2::geom_point(aes(x = Time, y = Value))+
  ggplot2::geom_line(aes(x = Time, y = Value))+
  # ecodata::geom_lm(aes(x = Time, y = Value,
  #              group = Var))+
  #ggplot2::geom_hline(aes(yintercept = hline),
  #         size = hline.size,
  #         alpha = hline.alpha,
  #         linetype = hline.lty)+
  ggplot2::scale_x_continuous(expand = c(0.05, 0.05)) +
  ggplot2::facet_wrap( ~ EPU)+
  ggplot2::ggtitle("Ryther Index")+
  ggplot2::ylab("mt km-2 y-1")+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()+
  ecodata::theme_facet()
