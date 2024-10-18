
## Apex pred
apex<-ecodata::hms_landings %>% 
  dplyr::filter(stringr::str_detect(Var, "Revenue")) %>% 
  tidyr::separate(Var, c("Var", "trash"), sep = "_") %>% 
  #dplyr::mutate(Value = as.numeric(Value))%>% 
  dplyr::filter(!Var == "Smoothhound Sharks")


#Define constants for figure plot
series.col <- c("indianred","black")

##Plot
p1<-apex %>% 
  dplyr::filter(EPU == "NE") %>% 
  dplyr::group_by(Var) %>% 
  
  dplyr::mutate(Value = Value/1000000, 
    hline = mean(Value)) %>% 

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
  ggplot2::ylab(("Revenue (10^6 US Dollars)")) +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("HMS Revenue")+
  ecodata::theme_title()

p1
