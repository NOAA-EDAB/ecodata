
cumu <- ecodata::heatwave %>% 
  dplyr::filter(Var == "cumulative intensity-Surface") %>% 
  dplyr::mutate(Var = dplyr::recode(Var, "cumulative intensity-Surface" = "Cumulative Intensity (degree C x days)"))

maxin <- ecodata::heatwave %>% 
  dplyr::filter(Var == "maximum intensity-Surface") %>% 
  dplyr::group_by(Time, EPU, Var, Units) %>% 
  dplyr::summarise(Value = max(Value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Var = dplyr::recode(Var, "maximum intensity-Surface" = "Maximum Intensity (degree C)"))

cumud <- ecodata::heatwave %>% 
  dplyr::filter(Var == "cumulative intensity-SurfaceDetrended") %>% 
  dplyr::mutate(Var = dplyr::recode(Var, "cumulative intensity-SurfaceDetrended" = "Cumulative Intensity Detrended (degree C x days)"))

maxind <- ecodata::heatwave %>% 
  dplyr::filter(Var == "maximum intensity-SurfaceDetrended") %>% 
  dplyr::group_by(Time, EPU, Var, Units) %>% 
  dplyr::summarise(Value = max(Value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Var = dplyr::recode(Var, "maximum intensity-SurfaceDetrended" = "Maximum Intensity Detrended (degree C)"))


hw<- cumu %>%
  rbind(maxin, cumud, maxind) %>% 
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
