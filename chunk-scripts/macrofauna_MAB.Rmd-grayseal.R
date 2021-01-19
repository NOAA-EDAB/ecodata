
ecodata::grayseal %>% 
  dplyr::select(-Region, -Units) %>% 
  tidyr::drop_na() %>% 
  dplyr::group_by(YEAR) %>% 
  #tidyr::spread(key = Var, value = Value)
  tidyr::pivot_wider(names_from = Var, values_from = Value) %>% 
  ggplot2::ggplot()+ 
  ggplot2::geom_point(aes(x=YEAR, y = EST5, color = "black"))+
  ggplot2::geom_line(aes(x=YEAR, y = EST5, color = "black"))+
  ggplot2::geom_point(aes(x=YEAR, y = PBR, color = "red")) +
  ggplot2::geom_line(aes(x=YEAR, y = PBR, color = "red")) +
  ggplot2::geom_ribbon(aes(ymin = LCI, ymax = UCI, x = YEAR), alpha = 0.3)+
  ggplot2::geom_point(aes(x=YEAR, y = EST1, color = "blue")) +
  ggplot2::geom_line(aes(x=YEAR, y = EST1, color = "blue")) +
  ggplot2::ggtitle("Gray Seal Byctach")+
  ggplot2::ylab("Estimated Bycatch (n)")+
  ggplot2::scale_color_identity(name = "",
                          breaks = c("black", "red", "blue"),
                          labels = c("5-YR Estimate", "1-YR Estimate", "PBR"),
                          guide = "legend")+
  ggplot2::theme(legend.position = "bottom")+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()
