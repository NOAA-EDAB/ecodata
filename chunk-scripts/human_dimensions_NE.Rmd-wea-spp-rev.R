
ecodata::wind_revenue %>%
  tidyr::separate(Var, sep = ":", into = c("Species", "Var")) %>% 
  dplyr::filter(Var == "Sum of WEA_DOLLAR_TOTAL") %>% 
  dplyr::mutate(Value = Value/1000000,
                Time = as.integer(Time)) %>%
  dplyr::filter(EPU == "NE", 
                Species %in% c("HERRING, ATLANTIC","MONK","SCALLOPS/BUSHEL",
                               "HAKE, SILVER / WHITING",  "SKATE WINGS"  )) %>%
  dplyr::mutate(Species = recode(Species, "HERRING, ATLANTIC"="Atlantic Herring", 
                                 "MONK" = "Monkfish", 
                                 "SCALLOPS/BUSHEL" = "Sea Scallop", 
                                 "HAKE, SILVER / WHITING" = "Silver Hake",
                                  "SKATE WINGS"  = "Skates")) %>% 
  ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point(aes(x = Time, y = Value, color = Species))+
  ggplot2::geom_line(aes(x = Time, y = Value, color = Species))+
  # ecodata::geom_lm(aes(x = Time, y = Value, color = Species))+
  scale_x_continuous(breaks=c(2008,2012,2016, 2020))+
  ggplot2::ggtitle("Fishery Revenue in Wind Lease Areas")+
  ggplot2::ylab(expression("Dollars (10"^6*")"))+
  theme(legend.title = element_blank())+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
