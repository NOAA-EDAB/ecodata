
cal <- ecodata::calanus_stage %>% 
  
  tidyr::separate(Var, into = c("Var", "season"), sep = "-") %>% 
  dplyr::filter(EPU == "GOM", 
                Var %in% c( "c5", "adt")) %>%
  dplyr::mutate(Var = recode(Var, "c5" = "Stage 5", 
                             "adt" = "Adult" ))
  #dplyr::mutate(newval=ifelse(ndays<10, NA, Value),
  #              newday= (meanday/ 365))
  

cal$Var <- factor(cal$Var, levels = c( "Stage 5", "Adult"))
cal$season <- factor(cal$season, levels = c("Spring", "Summer", "Fall"))

cal %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, color = Var, fill = Var)) +
    ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  ggplot2::geom_bar(stat = "identity")+
  ggplot2::facet_wrap(~season)+
  ggplot2::ylab("Calanus Stage (N/100m^3)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("GOM Calanus Stage Abundance") +
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = element_blank())+
  ecodata::theme_facet()+
  scale_fill_manual(values = c("steelblue1", "coral1"))+
  scale_color_manual(values = c("steelblue1", "coral1"))+
  ecodata::theme_title()
