
zoo_fill <- expand.grid(Time = 2020, 
                       Value = NA, 
                       EPU = "MAB", 
                       Var = c("thecos_100m3", "pseudo_100m3"))
zoo_abund <- ecodata::zoo_regime %>% 
  dplyr::filter(EPU == "MAB",
         stringr::str_detect(Var, "thecos_100m3|pseudo_100m3"), 
         !Time == 2020) %>% 
  rbind(zoo_fill) %>% 
  dplyr::mutate(Var = dplyr::recode(Var, "thecos_100m3" = "Pteropods", 
              "pseudo_100m3" = "Pseudocalanus")) %>% 
  dplyr::group_by(Var, EPU) %>% 
  dplyr::mutate(Value = as.numeric(Value), 
                hline = mean(Value))


zoo_abund %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, color = Var)) +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls() +
  #ecodata::geom_lm(aes(x = Time, y = Value))+
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::ylab(expression("Abundance Anomaly")) +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Zooplankton abundance anomaly") +
  #ggplot2::facet_wrap(Var~., ncol = 3) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  ggplot2::geom_hline(aes(yintercept = 0),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0,
                                face = "italic"), 
                 legend.title = element_blank(), 
                 legend.position = "bottom")+
  ecodata::theme_title()
