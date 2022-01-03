
cpi<- ecodata::cold_pool %>% 
  dplyr::filter(stringr::str_detect(Var, pattern = "cold_pool")) %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value)  %>% 
  dplyr::mutate(Upper = cold_pool_index + se_cold_pool_index, 
                Lower = cold_pool_index - se_cold_pool_index) %>%
  dplyr::select(!se_cold_pool_index)%>%
  dplyr::rename(Value = cold_pool_index)%>%
  dplyr::mutate(Var = c("cold_pool_index")) %>% 
  ggplot2::ggplot() + 
 #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
  # ggplot2::geom_ribbon(aes(x = Time, ymin = Lower, ymax = Upper), fill = "gray")+
  ggplot2::geom_hline(aes(yintercept = 0))+
  ecodata::geom_gls(aes(x = Time, y = Value))+
  ggplot2::ylab("Cold Pool Index (x(-1))") +
  #ggplot2::scale_y_reverse()+
  ggplot2::xlab("")+
  ecodata::theme_ts()+
  ecodata::theme_title()+
  ggplot2::annotate("segment", x = 2022, xend = 2022, y = 0.05, yend = 2,
           colour = "blue", size = 0.70, arrow = arrow())+
  ggplot2::annotate("segment", x = 2022, xend = 2022, y = -0.05, yend = -2,
           colour = "red", size = 0.70, arrow = arrow())+
  ggplot2::annotate("text", x = 2022, y = 2.2, label = "Colder", size = 2,colour = "blue")+
   ggplot2::annotate("text", x = 2022, y = -2.2, label = "Warmer",size = 2, colour = "red")


  
  
ei<- ecodata::cold_pool %>% 
  dplyr::filter(stringr::str_detect(Var, pattern = "extent")) %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value)  %>% 
  dplyr::mutate(Upper = extent_index + se_extent_index, 
                Lower = extent_index - se_extent_index) %>%
  dplyr::select(!se_extent_index)%>%
  dplyr::rename(Value = extent_index)%>%
  dplyr::mutate(Var = c("extent_index")) %>% 
  ggplot2::ggplot() + 
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
  ggplot2::geom_hline(aes(yintercept = 0))+
  ecodata::geom_gls(aes(x = Time, y = Value))+
  ggplot2::ylab("Spatial Extent Index") +
  ggplot2::xlab("")+
  ecodata::theme_ts()+
  ecodata::theme_title()+
  ggplot2::annotate("segment", x = 2022, xend = 2022, y = 0.05, yend = 50,
           colour = "blue", size = 0.70, arrow = arrow())+
  ggplot2::annotate("segment", x = 2022, xend = 2022, y = -0.05, yend = -250,
           colour = "red", size = 0.70, arrow = arrow())+
  ggplot2::annotate("text", x = 2022, y = 52, label = "Larger",size = 2, colour = "blue")+
   ggplot2::annotate("text", x = 2022, y = -255, label = "Smaller",size = 2, colour = "red")


pi<- ecodata::cold_pool %>% 
  dplyr::filter(stringr::str_detect(Var, pattern = "persistence")) %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value)  %>% 
  dplyr::mutate(Upper = persistence_index + se_persistence_index, 
                Lower = persistence_index - se_persistence_index) %>%
  dplyr::select(!se_persistence_index)%>%
  dplyr::rename(Value = persistence_index)%>%
  dplyr::mutate(Var = c("persistence_index"))%>% 
  ggplot2::ggplot() + 
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
  ggplot2::geom_hline(aes(yintercept = 0))+
  ecodata::geom_gls(aes(x = Time, y = Value))+
  ggplot2::ylab("Persistence Index") +
  ggplot2::xlab("")+
  ecodata::theme_ts()+
  ecodata::theme_title()+
  ggplot2::annotate("segment", x = 2022, xend = 2022, y = 0.01, yend = 0.6,
           colour = "blue", size = 0.70, arrow = arrow())+
  ggplot2::annotate("segment", x = 2022, xend = 2022, y = -0.05, yend = -1.5,
           colour = "red", size = 0.70, arrow = arrow())+
  ggplot2::annotate("text", x = 2022, y = 0.7, label = "Longer", size = 2,colour = "blue")+
   ggplot2::annotate("text", x = 2022, y = -1.6, label = "Shorter", size = 2, colour = "red")

#cowplot::plot_grid(cpi, pi, ei, labels = c('a', 'b', 'c'), align = "h")

gridExtra::grid.arrange(cpi, pi,ei, ncol=3)
