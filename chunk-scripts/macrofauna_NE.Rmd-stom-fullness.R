
gb_fullness <- ecodata::stom_fullness %>%
  dplyr::group_by(Var, EPU) %>% ## Remove values with missing data
  dplyr::filter(n()> 10) %>% ## at least tens years
  dplyr::ungroup() %>% 
  dplyr::filter(EPU == "GB") %>%
  ggplot2::ggplot(aes(x = Time, y = Value)) +
  ggplot2::geom_line() +
  #geom_point() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::ggtitle("GB Stomach fullness") +
  ggplot2::ylab("Stomach fullness anomaly") +
  ggplot2::facet_wrap(~Var)+
  ggplot2::theme(strip.text=element_text(hjust=0), 
        legend.position = "none", 
        axis.text.x = element_text(angle = 45))

gom_fullness <- ecodata::stom_fullness %>%
  dplyr::group_by(Var, EPU) %>% ## Remove values with missing data
  dplyr::filter(n()> 10) %>% ## at least tens years
  dplyr::ungroup() %>% 
  dplyr::filter(EPU == "GOM") %>%
  ggplot2::ggplot(aes(x = Time, y = Value)) +
  ggplot2::geom_line() +
  #geom_point() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::ggtitle("GOM Stomach fullness") +
  ggplot2::ylab("Stomach fullness anomaly") +
  ggplot2::facet_wrap(~Var)+
  ggplot2::theme(strip.text=element_text(hjust=0), 
        legend.position = "none", 
        axis.text.x = element_text(angle = 45))

gb_fullness
gom_fullness
