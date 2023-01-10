
ecodata::seal_pups %>%
  tidyr::separate(Var, into = c("Var", "colony"), sep = "-") %>% 
  dplyr::filter(!colony == "Green") %>% 
  dplyr::mutate(Var = dplyr::recode(Var, "log" = "Log Counts", "count" = "Counts"), 
                colony = dplyr::recode(colony, "Nomans.Land" = "Nomans Land")) %>% 
  ggplot()+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point(aes(x=Time, y = Value,color = colony, shape = colony))+
  ggplot2::facet_wrap(~Var, scales = "free")+
  ggplot2::ggtitle("Estimated Gray Seal Pup Births")+
  ggplot2::ylab("Pup Count")+
  ggplot2::xlab(element_blank())+
  #ggplot2::scale_color_discrete(name = "Category")+
  ecodata::theme_ts()+
  ecodata::theme_title()+
  ecodata::theme_facet()+
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = element_blank())
