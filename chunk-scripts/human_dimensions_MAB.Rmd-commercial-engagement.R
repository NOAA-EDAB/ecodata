
com<-ecodata::engagement %>% 
  dplyr::filter(Region == "MAB", 
                Fishery == "Commercial") %>% 
  dplyr::rename("EJRating" = "EJ Rating")

com2<-com %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x = Eng, y = Rel, color = EJRating), size = 2)+
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed",color = "black", size = 0.5)+
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.5) +
  ggrepel::geom_text_repel(aes(x = Eng, #geom_text_repel auto-jitters text around points
                      y = Rel,
                      label = Community,
                      color = EJRating), show.legend = FALSE, direction = "both", box.padding = 0.2, size = 3)+
  ggplot2::scale_color_brewer(palette = "Dark2", #Change legend labels for clarity
                     breaks = com$EJRating) +
  xlim(-2,12.7)+
  ylim(-1,3.0)+
  theme(legend.position=c(0.75, 0.85), 
        legend.title = element_blank(),       
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  ggplot2::xlab("Commercial Engagement Index") +
  ggplot2::ylab("Commercial Reliance Index") +
  ggplot2::ggtitle("Environmental Justice in Top Commercial Fishing Communities")+
  #ggplot2::guides(color = FALSE) +
  #theme_bw()
  ecodata::theme_ts()+
  ecodata::theme_title()
  #ecodata::theme_facet()
  
  
  gridExtra::grid.arrange(com2, bottom = textGrob("Low <---------------------------------------------------------------------------------------------------------------------------> High", 
                                     x = 0.5, y = 1, gp = gpar(fontsize = 7)),
                          left = textGrob("Low <--------------------------------------------------------------------------------------> High", rot = 90,
                                   x = 1, y = 0.45, gp = gpar(fontsize = 7)))
