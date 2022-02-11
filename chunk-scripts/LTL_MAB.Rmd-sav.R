
ecodata::SAV %>% 
  dplyr::filter(!Var == "Baywide", 
                !Var == "Oligohaline", 
                !Var == "Mesohaline") %>% 
  dplyr::mutate(Var = recode(Var, 
                             "Tidal" = "Tidal Fresh", 
                             "Polyhaline" = "High Salinity"), 
                Var = factor(Var, levels = c("Tidal Fresh", "High Salinity"))) %>% 
  dplyr::mutate(Value = (Value/1000)) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value))+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::facet_wrap(~Var)+
  ecodata::geom_gls(aes(x = Time, y = Value)) +
  ecodata::theme_ts()+
  ggplot2::theme(strip.text=element_text(hjust=0,
                                face = "italic"), 
        axis.title.y = element_text(angle = 90), 
        legend.title = element_blank()) +
  # ggplot2::scale_color_discrete(name = "",
  # labels = c("Lower bay",  "Upper Bay"))+
  ecodata::theme_title()+
  ggplot2::ylab(expression("Acres (10"^3*")"))+
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Submerged Aquatic Vegetation (SAV) Abundance")+
  ecodata::theme_facet()
