
sw.df <- ecodata::slopewater %>% 
  dplyr::mutate(Var, Var = plyr::mapvalues(Var, from = c("WSW proportion ne channel",
                                                  "LSLW proportion ne channel"),
                                    to = c("WSW","LSW"))) %>% 
  dplyr::rename(Flavor  = Var) %>% 
  dplyr::group_by(Flavor) %>% 
  dplyr::mutate(hline = mean(Value)) 

sw.df$Origin <- factor(sw.df$Flavor, levels = c("WSW","LSW"))

ggplot2::ggplot(data = sw.df) +
  ggplot2::geom_line(aes(x = Time, y = Value, color = Origin))+
  ggplot2::geom_point(aes(x = Time, y = Value, color = Origin)) +
  ggplot2::ylab("Percent of Total Slopewater") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Slopewater Proportions in NE Channel")+
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  ggplot2::geom_hline(aes(yintercept = hline, color = Origin),
           size = hline.size, alpha = hline.alpha,
           linetype = hline.lty)+
  ecodata::theme_ts() +
  ecodata::theme_title()
