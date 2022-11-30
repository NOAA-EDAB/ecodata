
ecodata::rec_hms %>% 
  dplyr::filter(EPU == "MAB") %>%
  tidyr::separate(Var, c("Var", "X"), sep = "-") %>%
  dplyr::mutate(Value = Value/10000) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x = Time, y = Value, color = Var))+
  ggplot2::geom_line(aes(x = Time, y = Value, color = Var))+
  ggplot2::ylab(expression("Catch (N"^4*")"))+
  ggplot2::ggtitle("Recreational Shark Landings")+
  ggplot2::xlab(element_blank())+
  ggplot2::theme(legend.title = element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
