
prop <- ecodata::nefsc_survey_disaggregated %>% 
  dplyr::filter(!stringr::str_detect(`Feeding guild`, "Other|other"),
         !is.na(Proportion)) %>% #remove "other"
  tidyr::unite(.,Var,c("Feeding guild","Season"), sep = " ") %>% 
  dplyr::filter(!stringr::str_detect(Var,"Apex|Benthos"),
         !is.na(Management)) %>% #remove benthos and apex predators
  dplyr::group_by(EPU, Management, Var, Time) %>%
  dplyr::summarise(Proportion = sum(Proportion)) %>% 
  dplyr::mutate(Var2 = paste(Management,Var),
         tt = paste(Management,round(Proportion,2), Time),
         Proportion = ifelse(Proportion == 0, NA, Proportion)) %>% #tt = tooltip
  dplyr::group_by(EPU,Var2) %>% 
  dplyr::mutate(hline = mean(Proportion,na.rm = T)) %>% 
  tidyr::complete(Time = full_seq(min(.$Time):max(.$Time),1),
           tidyr::nesting(Management, Var)) 


#Change factor levels
prop$Var <- factor(prop$Var,levels = c("Piscivore spring","Piscivore fall",
                                       "Benthivore spring","Benthivore fall",            
                                       "Planktivore spring", "Planktivore fall" ))

GB_prop <- 
  prop %>% 
    dplyr::filter(EPU == "GB") %>% 
ggplot2::ggplot(aes(x = Time, y = Proportion, color = Management, group = Var2)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
           xmin = x.shade.min , xmax = x.shade.max ,
           ymin = -Inf, ymax = Inf) +
  
  #Add time series
  ggplot2::geom_line(size = 0.5) +
  ggiraph::geom_point_interactive(aes(tooltip = tt),size = 0.5) +
#  guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var2,
                 color = Management),
             size = 0.5,
             alpha = hline.alpha,
             linetype = hline.lty)+
  
  #Facet 
  ggplot2::facet_wrap(Var~., ncol = 2) +
  
  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Proportion of survey")) +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("GB") +
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0))
GB_prop <- ggiraph::girafe(ggobj = GB_prop)
GB_prop <- ggiraph::girafe_options(GB_prop, opts_zoom(max = 5) )
GB_prop
