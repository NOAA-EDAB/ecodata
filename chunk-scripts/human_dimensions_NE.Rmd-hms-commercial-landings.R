
#Get data for plotting
## Apex pred
apex<-ecodata::hms_landings %>% 
  dplyr::filter(stringr::str_detect(Var, "Landings")) %>%
  #dplyr::mutate(Value = as.numeric(Value)) %>% 
  separate(Var, c("Var", "trash"), sep = "_") #%>% 
  #dplyr::filter(!Var == "Smoothhound Sharks")

##Plot
p1<-apex %>% 
  dplyr::filter(EPU == "NE") %>% 
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value)) %>% 

  ggplot2::ggplot(aes(x = Time, y = Value, color = Var)) +
  ggplot2::geom_line(size = lwd) +
  ggplot2::geom_point(size = pcex) +
  # ggplot2::geom_hline(aes(yintercept = hline,
  #                color = Var,
  #                size = Var),
  #            size = hline.size,
  #            alpha = hline.alpha,
  #            linetype = hline.lty)+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.title = element_blank())+
  ggplot2::ylab("Landings (metric tons)")+
  ggplot2::ggtitle("HMS Commercial Landings")
  ggplot2::xlab(element_blank())+
  ecodata::theme_title()

p1
