
cumu <- ecodata::heatwave %>% 
  dplyr::filter(Var == "cumulative intensity") %>% 
  dplyr::mutate(Var = dplyr::recode(Var, "cumulative intensity" = "Cumulative Intensity (degree C x days)"))

maxin <- ecodata::heatwave %>% 
  dplyr::filter(Var == "maximum intensity") %>% 
  dplyr::group_by(Time, EPU, Var, Units) %>% 
  dplyr::summarise(Value = max(Value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Var = dplyr::recode(Var, "maximum intensity" = "Maximum Intensity (degree C)"))

hw<- cumu %>%
  rbind(maxin) %>% 
  dplyr::group_by(Var, EPU) %>% 
  dplyr::mutate(hline = mean(Value))

mab.hw<- hw %>% dplyr::filter(EPU == epu_abbr)
mab.hw %>% 
  ggplot2::ggplot() +
  ggplot2::geom_line(aes(x = Time, y = Value)) +
  ggplot2::geom_point(aes(x = Time, y = Value)) +
  ecodata::geom_gls(aes(x = Time, y = Value, group = Var)) +
  #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  ggplot2::ylab("") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Mid-Atlantic Marine Heatwave Intesity") +
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
