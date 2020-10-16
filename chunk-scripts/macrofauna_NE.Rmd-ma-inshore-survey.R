
mass <- ecodata::mass_inshore_survey %>% 
  dplyr::filter(EPU == "GB",
         Time>1980) %>% 
  tidyr::separate(Var, c("feeding.guild", "season", "biomass", "stat")) %>% 
  tidyr::unite(.,Var, c("feeding.guild","season"),sep = " ") %>% 
  dplyr::group_by(Time, Var) %>% 
  tidyr::spread(stat, Value) %>% 
  dplyr::mutate(upper = Index + (2*SD), 
         lower = Index - (2*SD)) %>% 
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Index))
mass$Var <- factor(mass$Var,levels = c("Piscivore Spring","Piscivore Fall",
                                       "Benthivore Spring","Benthivore Fall",
                                       "Planktivore Spring","Planktivore Fall",
                                       "Benthos Spring","Benthos Fall")) 
###Plot 1
p1<-mass %>% 
  dplyr::filter(!is.na(Var), 
         stringr::str_detect(Var, "Piscivore")) %>% 
  ggplot2::ggplot(aes(x = Time, y = Index)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  ecodata::geom_gls(aes(x = Time, y = Index,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
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
  ggplot2::ggtitle("MA Inshore BTS") +
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

###Plot 2
p2<-mass %>% 
  dplyr::filter(!is.na(Var), 
         str_detect(Var, "Benthivore")) %>% 
  ggplot2::ggplot(aes(x = Time, y = Index)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  ecodata::geom_gls(aes(x = Time, y = Index,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
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
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

###Plot 3
p3<-mass %>% 
  dplyr::filter(!is.na(Var), 
         str_detect(Var, "Planktivore")) %>% 
  ggplot2::ggplot(aes(x = Time, y = Index)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  ecodata::geom_gls(aes(x = Time, y = Index,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
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
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

###Plot 4
p4<-mass %>% 
  dplyr::filter(!is.na(Var), 
         str_detect(Var, "Benthos")) %>% 
  ggplot2::ggplot(aes(x = Time, y = Index)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  ecodata::geom_gls(aes(x = Time, y = Index,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
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
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0))

cowplot::plot_grid(p1, p2, p3, p4, nrow = 4)
