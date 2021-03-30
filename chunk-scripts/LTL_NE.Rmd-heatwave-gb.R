
cumu <- ecodata::heatwave %>% 
  dplyr::filter(Var == "cumulative intensity") %>% 
  dplyr::mutate(Var = recode(Var, "cumulative intensity" = "Cumulative Intensity (degree C x days)"))

maxin <- ecodata::heatwave %>% 
  dplyr::filter(Var == "maximum intensity") %>% 
  dplyr::group_by(Time, EPU, Var, Units) %>% 
  dplyr::summarise(Value = max(Value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Var = recode(Var, "maximum intensity" = "Maximum Intensity (degree C)"))

hw<- cumu %>%
  rbind(maxin) %>% 
  dplyr::group_by(Var, EPU) %>% 
  dplyr::mutate(hline = mean(Value))

gb.hw<-hw %>% dplyr::filter(EPU == "GB") %>% 
  ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = Time, y = Value)) +
  ggplot2::geom_point(aes(x = Time, y = Value)) +
  ecodata::geom_gls(aes(x = Time, y = Value)) +
  ggplot2::ylab("") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Georges Bank") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+

  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ggplot2::facet_wrap(~Var, scales = "free") +
  ecodata::theme_ts()+
  ggplot2::theme(strip.text=element_text(hjust=0,
                                face = "italic"), 
        axis.title.y = element_text(angle = 90))+
  ecodata::theme_title()+
  ecodata::theme_facet()

gb.hw
