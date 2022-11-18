
upper.line<-ecodata::wcr %>%
  dplyr::filter(Time>2000) %>% 
  dplyr::mutate(hline = c(mean(Value)))
lower.line<-ecodata::wcr%>%
  dplyr::filter(Time<2000) %>% 
  dplyr::mutate(hline = c(mean(Value)))
wcr<- upper.line %>% 
  rbind(lower.line)

wcr %>% 
  ggplot2::ggplot(aes(x = Time, y = Value))+
    ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  ggplot2::ylab("Warm Core Ring Births")+
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Warm Core Rings")+
  ecodata::theme_ts()+
  ggplot2::geom_segment(data = upper.line, aes(x = min(Time), y = hline, 
                                      xend = max(Time), yend = hline, color = "segment") )+
  ggplot2::geom_segment(data = lower.line, aes(x = min(Time), y = hline, 
                                    xend = max(Time), yend = hline, color = "segment") )+
  ggplot2::theme(legend.position = "none")+
  ecodata::theme_title()
