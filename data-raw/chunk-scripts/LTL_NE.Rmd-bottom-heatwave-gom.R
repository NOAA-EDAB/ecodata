
hw1<- ecodata::heatwave %>%
  dplyr::filter(Time >= 2021) %>% 
  dplyr::mutate(Source = c("PSY"))
hwts<- ecodata::heatwave %>% 
  dplyr::filter(Time <= 2020) %>% 
  dplyr::mutate(Source = c("Glorys")) %>% 
  rbind(hw1) 

durd <- hwts %>% 
  dplyr::filter(Var == "duration-BottomDetrended") %>% 
  dplyr::group_by(Time, EPU, Var, Units, Source) %>% 
  dplyr::summarise(Value = sum(Value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Var = dplyr::recode(Var, "duration-BottomDetrended" = "Total Days Detrended (N days)"))


maxind <- hwts %>% 
  dplyr::filter(Var == "maximum intensity-BottomDetrended") %>% 
  dplyr::group_by(Time, EPU, Var, Units, Source) %>% 
  dplyr::summarise(Value = max(Value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Var = dplyr::recode(Var, "maximum intensity-BottomDetrended" = "Maximum Intensity Detrended (degree C)"))

hw<- durd %>%
  rbind(maxind) %>% 
  dplyr::group_by(Var, EPU) %>% 
  dplyr::mutate(hline = mean(Value))

mab.hw<- hw %>% dplyr::filter(EPU == "GOM")
mab.hw %>% 
  ggplot2::ggplot() +
  ggplot2::geom_line(aes(x = Time, y = Value)) +
  ggplot2::geom_point(aes(x = Time, y = Value,  shape = Source)) +
  ggplot2::scale_shape_manual(values = c(16, 1)) + 
  ecodata::geom_gls(aes(x = Time, y = Value))+ 
  #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  ggplot2::ylab("") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Gulf of Maine Marine Bottom Temp Heatwave Intesity") +
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
  ecodata::theme_title()+
  ecodata::theme_facet()
