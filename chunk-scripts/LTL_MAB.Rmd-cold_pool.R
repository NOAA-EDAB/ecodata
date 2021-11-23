
cpi<- ecodata::cold_pool %>% 
  dplyr::filter(stringr::str_detect(Var, pattern = "cold_pool")) %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value)  %>% 
  dplyr::mutate(Upper = cold_pool_index + se_cold_pool_index, 
                Lower = cold_pool_index - se_cold_pool_index) %>%
  dplyr::select(!se_cold_pool_index)%>%
  dplyr::rename(Value = cold_pool_index)%>%
  dplyr::mutate(Var = c("cold_pool_index"))
  
  
ei<- ecodata::cold_pool %>% 
  dplyr::filter(stringr::str_detect(Var, pattern = "extent")) %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value)  %>% 
  dplyr::mutate(Upper = extent_index + se_extent_index, 
                Lower = extent_index - se_extent_index) %>%
  dplyr::select(!se_extent_index)%>%
  dplyr::rename(Value = extent_index)%>%
  dplyr::mutate(Var = c("extent_index"))

pi<- ecodata::cold_pool %>% 
  dplyr::filter(stringr::str_detect(Var, pattern = "persistence")) %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value)  %>% 
  dplyr::mutate(Upper = persistence_index + se_persistence_index, 
                Lower = persistence_index - se_persistence_index) %>%
  dplyr::select(!se_persistence_index)%>%
  dplyr::rename(Value = persistence_index)%>%
  dplyr::mutate(Var = c("persistence_index"))

cp<- rbind(cpi, ei, pi)

cp %>% ggplot2::ggplot() + 
 #Highlight last ten years
  #ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #    xmin = x.shade.min , xmax = x.shade.max) +
  ecodata::geom_gls(aes(x = Time, y = Value),
             alpha = trend.alpha, size = trend.size) +
  ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
  ggplot2::geom_ribbon(aes(x = Time, ymin = Lower, ymax = Upper), fill = "gray")+
  ggplot2::geom_hline(aes(yintercept = 0))+
  #   size = hline.size,
   #  alpha = hline.alpha,
    # linetype = hline.lty)+
  ggplot2::ggtitle("Cold Pool Index")+
  ggplot2::facet_wrap(~Var, scales = "free")+
  ggplot2::ylab("Cold Pool") +
  ggplot2::xlab("")+
  ecodata::theme_ts()+
  ecodata::theme_title()
