
bt1<- ecodata::bottom_temp_model_anom %>%
  dplyr::filter(Time >= 2021) %>% 
  dplyr::mutate(Source = c("PSY"))
bt_ts<- ecodata::bottom_temp_model_anom %>% 
  dplyr::filter(Time <= 2020) %>% 
  dplyr::mutate(Source = c("Glorys")) %>% 
  rbind(bt1) %>% 
  tidyr::separate(Var, into = c("season"), sep = "_")

bt_ts$season <- factor(bt_ts$season, levels = c("Winter",
                                            "Spring",
                                            "Summer",
                                            "Fall"))

bt_ts %>%  dplyr::filter(EPU == "MAB",
                         !season == "Annual") %>% 
  dplyr::mutate(hline = mean(Value), 
                Time = as.numeric(Time)) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value)) +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
                    xmin = x.shade.min , xmax = x.shade.max,
                    ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line() +
  ggplot2::geom_point(aes(shape = Source)) +
  ggplot2::scale_shape_manual(values = c(16, 1))+
  ecodata::geom_gls(alpha = trend.alpha + 0.25) +
  ggplot2::ylab("BT anomaly (C)")+
  ggplot2::xlab(element_blank())+
  ggplot2::facet_wrap(~season, ncol = 2)+
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::geom_hline(aes(yintercept = hline), 
                      size = hline.size,
                      alpha = hline.alpha,
                      linetype = hline.lty) +
  ecodata::theme_ts()
