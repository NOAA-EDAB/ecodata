
resca_gm<- ecodata::forage_index %>% 
  dplyr::filter(Var %in% c("Fall Forage Fish Biomass Estimate", 
                           "Spring Forage Fish Biomass Estimate"), 
                EPU %in% c("GOM")) %>% 
  dplyr::summarise(max = max(Value)) 

gm<- ecodata::forage_index %>% 
  dplyr::filter(Var %in% c("Fall Forage Fish Biomass Estimate", 
                           "Fall Forage Fish Biomass Estimate SE",
                           "Spring Forage Fish Biomass Estimate",
                           "Spring Forage Fish Biomass Estimate SE"), 
                EPU %in% c("GOM")) %>% 
  tidyr::separate(Var, into = c("Season", "A", "B", "C", "D", "Var")) %>% 
  dplyr::mutate(Var = replace_na(Var, "Mean"), 
                max = as.numeric(resca_gm)) %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value) %>% 
  dplyr::mutate(#Value = Value/resca, 
    Mean = as.numeric(Mean), 
                Mean = Mean/max,
                SE = SE/max,
                Upper = Mean + SE, 
                Lower = Mean - SE) %>% 
  ggplot2::ggplot(aes(x = Time, y = Mean, group = Season))+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Season), alpha = 0.5)+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::ggtitle("")+
  ggplot2::ylab(element_blank())+
  ggplot2::xlab(element_blank())+
  ggplot2::facet_wrap(.~EPU)+
  ecodata::geom_gls()+
  ecodata::theme_ts()+
  ecodata::theme_facet()+
  ecodata::theme_title()

resca_gb<- ecodata::forage_index %>% 
  dplyr::filter(Var %in% c("Fall Forage Fish Biomass Estimate", 
                           "Spring Forage Fish Biomass Estimate"), 
                EPU %in% c("GB")) %>% 
  dplyr::summarise(max = max(Value)) 

gb<- ecodata::forage_index %>% 
  dplyr::filter(Var %in% c("Fall Forage Fish Biomass Estimate", 
                           "Fall Forage Fish Biomass Estimate SE",
                           "Spring Forage Fish Biomass Estimate",
                           "Spring Forage Fish Biomass Estimate SE"), 
                EPU %in% c("GB")) %>% 
  tidyr::separate(Var, into = c("Season", "A", "B", "C", "D", "Var")) %>% 
  dplyr::mutate(Var = replace_na(Var, "Mean"), 
                max = as.numeric(resca_gb)) %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value) %>% 
  dplyr::mutate(#Value = Value/resca, 
    Mean = as.numeric(Mean), 
                Mean = Mean/max,
                SE = SE/max,
                Upper = Mean + SE, 
                Lower = Mean - SE) %>% 
  ggplot2::ggplot(aes(x = Time, y = Mean, group = Season))+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Season), alpha = 0.5)+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::ggtitle("Forage Biomass Index")+
  ggplot2::ylab(expression("Relative Forage Biomass"))+
  ggplot2::xlab(element_blank())+
  ggplot2::facet_wrap(.~EPU)+
  ggplot2::theme(legend.position = "none")+
  ecodata::geom_gls()+
  ecodata::theme_ts()+
  ecodata::theme_facet()+
  ecodata::theme_title()

cowplot::plot_grid( gb, gm,  nrow=1)
