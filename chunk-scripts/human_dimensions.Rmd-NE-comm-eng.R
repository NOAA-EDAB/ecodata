
eng<-ecodata::engagement %>% 
  dplyr::filter(!Var == "med.high.scores")
eng$Var <- factor(eng$Var, levels = c("%High","%Medium High","%Moderate", "%Low"))
eng %>% dplyr::filter(EPU == "NE") %>% 
  ggplot2::ggplot()+
   #ylim(0.8, NA)+
  ggplot2::geom_bar(aes(x = Time, y = Value, 
               fill = Var), 
           stat = "identity")+
  #scale_y_continuous(labels = Value(suffix = "%", prefix = "")) +
  #geom_text(aes(x = Time, y = Value,
  #             label = paste0(Value,"%")), size=4) +
  ggplot2::theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank())+
  ggplot2::coord_cartesian(ylim=c(0.75,1))+
  ggplot2::xlab("Time") +
  ggplot2::ylab("% Communities in each category (Low to High)")+
  ggplot2::ggtitle("Commercial Engagement")+
  ecodata::theme_ts()


ecodata::engagement %>% 
  dplyr::filter(Var == "med.high.scores", 
         EPU == "NE") %>% 
  dplyr::mutate(hline = mean(Value)) %>% 
  ggplot2::ggplot()+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::ggtitle("Medium-High communities ") +
  ggplot2::ylab(expression("Average score for Med High communities")) +
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts()
