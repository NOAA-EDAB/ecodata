
menh <- ecodata::ne_inshore_survey %>% 
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value))
  

menh$Var <- factor(menh$Var,levels = c("Piscivore Spring","Piscivore Fall",
                                       "Benthivore Spring","Benthivore Fall",            
                                       "Planktivore Spring","Planktivore Fall",             
                                       "Benthos Spring",  "Benthos Fall"))
p1<-menh %>% 
  dplyr::filter(str_detect(Var, "Piscivore")) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, color = Var)) +
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Add time series
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  ggplot2::facet_wrap(Var~., ncol = 2) +
  
  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("ME/NH Inshore BTS") +
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

p2<-menh %>% 
  dplyr::filter(str_detect(Var, "Benthivore")) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, color = Var)) +
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Add time series
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  ggplot2::facet_wrap(Var~., ncol = 2) +
  
  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

p3<-menh %>% 
  dplyr::filter(str_detect(Var, "Planktivore")) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, color = Var)) +
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Add time series
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  ggplot2::facet_wrap(Var~., ncol = 2) +
  
  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

p4<-menh %>% 
  dplyr::filter(str_detect(Var, "Benthos")) %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, color = Var)) +
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Add time series
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  ggplot2::facet_wrap(Var~., ncol = 2) +
  
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0))

cowplot::plot_grid(p1, p2, p3, p4, nrow = 4)
