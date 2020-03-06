
fullness <- ecodata::stom_fullness %>%
  dplyr::group_by(Var, EPU) %>% ## Remove values with missing data
  dplyr::filter(n()> 10) %>% ## at least ten years of data
  dplyr::ungroup() %>% 
  dplyr::filter(EPU == "MAB") %>%
  ggplot2::ggplot(aes(x = Time, y = Value)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::ggtitle("Stomach fullness") +
  ggplot2::ylab("Stomach fullness") +
  ggplot2::facet_wrap(~Var)+
  ggplot2::theme(strip.text=element_text(hjust=0))

fullness
