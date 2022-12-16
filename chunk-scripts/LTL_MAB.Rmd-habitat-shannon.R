
MAB<- ecodata::habitat_diversity %>% 
  dplyr::filter(Var == "Shannon",
                EPU == "MAB") %>% 
  dplyr::group_by(EPU) %>% 
  dplyr::mutate(mean = as.numeric(Value),
                Time = as.numeric(Time),
                hline = mean(Value))

ecodata::habitat_diversity %>% 
  dplyr::filter(Var == "Shannon") %>% 
  dplyr::group_by(EPU) %>% 
  dplyr::mutate(mean = as.numeric(Value),
                Time = as.numeric(Time),
                hline = mean(Value)) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, color = EPU))+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point()+
  ggplot2::geom_line() +
  ggplot2::geom_point(data = MAB, aes(x = Time, y = mean), size = 2)+
  ggplot2::geom_line(data = MAB, aes(x = Time, y = mean), size = 2) +
  ggplot2::ylab("") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Mid-Atlantic Species Shannon Diversity from Habitat Assessment") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  # ggplot2::geom_hline(aes(yintercept = hline),
  #          size = hline.size,
  #          alpha = hline.alpha,
  #          linetype = hline.lty)+
  ecodata::theme_ts()+
  ggplot2::theme(strip.text=element_text(hjust=0,
                                face = "italic"))+
  ecodata::theme_title()
