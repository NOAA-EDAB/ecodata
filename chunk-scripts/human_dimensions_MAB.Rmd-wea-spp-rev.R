
ecodata::wind_revenue %>%
  tidyr::separate(Var, sep = "-", into = c("Species", "Var")) %>% 
  dplyr::filter(Var == "sum_landing") %>% 
  dplyr::mutate(Value = as.numeric(Value), 
                Value = Value/1000000,
                Time = as.integer(Time)) %>%
  dplyr::filter(EPU == "MAB", 
                Species %in% c("LONGFIN SQUID", "MONKFISH", "SUMMER FLOUNDER",
                               "OCEAN QUAHOG", "SURFCLAM")) %>% 
                # Species %in% c("SQUID / LOLIGO","MONK","FLOUNDER, SUMMER / FLUKE",
                #                "QUAHOGS/BUSHEL",  "CLAM, SURF/BUSHEL" )) %>%
  dplyr::mutate(Species = recode(Species, "LONGFIN SQUID"="Longfin Squid", 
                                 "MONKFISH" = "Monkfish", 
                                 "SUMMER FLOUNDER" = "Summer Flounder", 
                                 "OCEAN QUAHOG" = "Ocean Quahog",
                                 "SURFCLAM" = "Surfclam")) %>% 
  ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point(aes(x = Time, y = Value, color = Species))+
  ggplot2::geom_line(aes(x = Time, y = Value, color = Species))+
  scale_x_continuous(breaks=c(2008,2012,2016, 2020))+
  ggplot2::ggtitle("Fishery Revenue in Wind Lease Areas")+
  ggplot2::ylab(expression("Dollars (10"^6*")"))+
  theme(legend.title = element_blank())+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
