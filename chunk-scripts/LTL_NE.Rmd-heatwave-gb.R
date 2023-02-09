
durd <- ecodata::heatwave %>% 
  dplyr::filter(Var == "duration-SurfaceDetrended", 
                Time > 1982) %>% 
  dplyr::group_by(Time, EPU, Var, Units) %>% 
  dplyr::summarise(Value = sum(Value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Var = dplyr::recode(Var, "duration-SurfaceDetrended" = "Total Days Detrended (N days)"))
  


maxind <- ecodata::heatwave %>% 
  dplyr::filter(Var == "maximum intensity-SurfaceDetrended") %>% 
  dplyr::group_by(Time, EPU, Var, Units) %>% 
  dplyr::summarise(Value = max(Value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Var = dplyr::recode(Var, "maximum intensity-SurfaceDetrended" = "Maximum Intensity Detrended (degree C)"))


hw<- durd %>%
  rbind( maxind) %>% 
  dplyr::group_by(Var, EPU) %>% 
  dplyr::mutate(hline = mean(Value))

gb.hw<- hw %>% dplyr::filter(EPU == "GB")
gb.hw %>% 
  ggplot2::ggplot() +
  ggplot2::geom_line(aes(x = Time, y = Value)) +
  ggplot2::geom_point(aes(x = Time, y = Value)) +
  ecodata::geom_gls(aes(x = Time, y = Value, group = Var)) +
  #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  ggplot2::ylab("") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Georges Bank Marine Heatwave Intesity") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::facet_wrap(~Var, scales = "free")+
  ecodata::theme_ts()+
  ggplot2::theme(strip.text=element_text(hjust=0,
                                face = "italic"))+
  ecodata::theme_title()
