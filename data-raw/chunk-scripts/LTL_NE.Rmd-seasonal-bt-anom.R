
bt1<- ecodata::bottom_temp_model_anom %>%
  dplyr::filter(Time >= 2021) %>% 
  dplyr::mutate(Source = c("PSY"))
bt_ts<- ecodata::bottom_temp_model_anom %>% 
  dplyr::filter(Time <= 2020) %>% 
  dplyr::mutate(Source = c("Glorys")) %>% 
  rbind(bt1) 

ne_anom <- bt_ts %>% 
  dplyr::filter(EPU %in% c("GB","GOM")) %>% 
  dplyr::mutate(Time = as.numeric(Time),
                hline = 0,
           Var = stringr::str_to_title(stringr::str_extract(Var,"Winter|Spring|Summer|Fall"))) %>% 
  dplyr::filter(!Var == "NA")
ne_anom$Var <- factor(ne_anom$Var, levels= c("Winter","Spring","Summer","Fall"))

ne_anom_plt <- ggplot2::ggplot(data = ne_anom, 
       aes(x = Time, y = Value, color = EPU, group = EPU)) +
  
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
                    xmin = x.shade.min , xmax = x.shade.max,
                    ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line()+
  ggplot2::geom_point(aes(shape = Source))+
  ggplot2::scale_shape_manual(values = c(16, 1)) + 
  ggplot2::ylim(-2,3)+
  ggplot2::ylab(expression("BT Anomaly (C)")) +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Gulf of Maine & Georges Bank BT Anomaly") +
  ggplot2::scale_color_manual(values = c("black","indianred"))+
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ggplot2::facet_wrap(Var ~., ncol = 2, scales = "free_y")+
  ecodata::theme_facet() +
  ecodata::geom_gls() +
  #ecodata::geom_lm(aes(x = Time, y = Value))+
  ggplot2::theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))+
  ecodata::theme_title()
ne_anom_plt
