
gom_larv_div <- ecodata::ichthyo_diversity %>%
  dplyr::filter(EPU == "GOM",
         str_detect(Var, "Ich_Shannon")) %>%
  dplyr::mutate(Var = word(Var,1)) %>% 
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value, na.rm = T)) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, group = Var)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
       annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls(aes(x = Time, y = mean(Value))) +
  ggplot2::ggtitle("GOM larval diversity") +
  ggplot2::ylab("Shannon Diversity") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0))

gb_larv_div <- ecodata::ichthyo_diversity %>%
  dplyr::filter(EPU == "GB",
         stringr::str_detect(Var, "Ich_Shannon")) %>%
  dplyr::mutate(Var = word(Var,1)) %>% 
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value, na.rm = T)) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, group = Var)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls(aes(x = Time, y = mean(Value))) +
  ggplot2::ggtitle("GB larval diversity") +
  ylab("Shannon Diversity") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
      geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0))

gb_larv_div + gom_larv_div + patchwork::plot_layout(ncol = 1)
