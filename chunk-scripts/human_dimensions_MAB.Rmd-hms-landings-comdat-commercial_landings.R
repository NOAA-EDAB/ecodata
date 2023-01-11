
## Apex pred
apex<-ecodata::hms_landings %>% 
  dplyr::filter(stringr::str_detect(Var, "Landings")) %>% 
  separate(Var, c("Var", "trash"), sep = "_")

##Plot
p1<-apex %>% 
  dplyr::filter(EPU == "MAB") %>% 
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value)) %>% 

  ggplot2::ggplot(aes(x = Time, y = Value, color = Var)) +
  ggplot2::geom_line(size = lwd) +
  ggplot2::geom_point(size = pcex) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 color = Var,
                 size = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.title = element_blank())+
  ggplot2::ylab(expression("Landings (metric tons)")) +
  ggplot2::xlab("Time")+
  ggplot2::ggtitle("HMS Landings")+
  ecodata::theme_title()

p1
