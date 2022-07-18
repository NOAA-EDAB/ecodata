
#Get NEAMAP
neamap <- ecodata::mab_inshore_survey %>%
  tidyr::separate(Var, into = c("Var",  "Val"), sep = "-") %>% 
  tidyr::pivot_wider(names_from = Val, values_from = Value) %>%  
  dplyr::mutate(Value = as.numeric(Value), 
                CV = as.numeric(CV)) %>% 
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value),
         SD = Value * CV, #calculate SD from CV
         upper = Value + (2*SD), 
         lower = Value - (2*SD))

neamap$Var <- factor(neamap$Var,levels = c("Piscivore Spring","Piscivore Fall",
                                           "Benthivore Spring", "Benthivore Fall",
                                           "Planktivore Spring", "Planktivore Fall",  
                                           "Benthos Spring", "Benthos Fall"))
## Piscivore 
neamap %>% 
  ggplot2::ggplot() +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  #Test for trend and add lines
  ecodata::geom_gls(aes(x = Time, y = Value,
               color = Var),
             alpha = trend.alpha, size = trend.size) +

  #Add time series
  ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") +
  ggplot2::geom_line(aes(x = Time, y = Value),size = lwd-0.5) +
  ggplot2::geom_point(aes(x = Time, y = Value),size = pcex-0.5) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
  ggplot2::facet_wrap(Var~.,ncol = 2) +

  #Axis and theme
  #ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  #ylim(0, 1200)+
  ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0), 
        axis.title.x=element_blank())
