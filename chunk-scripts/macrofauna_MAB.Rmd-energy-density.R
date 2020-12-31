
d<-ecodata::energy_density

old.ed<- data.frame("Species" = c("Atl. Herring","Atl. Herring","Illex squid", "Illex squid", "Sand lance","Sand lance",
                                  "Atl. Herring","Atl. Herring","Illex squid", "Illex squid", "Sand lance","Sand lance"),
                    "Year" = c("1980s", "1990s","1980s", "1990s","1980s", "1990s",
                               "1980s", "1990s","1980s", "1990s","1980s", "1990s"), 
                    "Season" = c("Spring", "Spring", "Spring", "Spring", "Spring", "Spring", 
                                     "Fall", "Fall", "Fall", "Fall", "Fall", "Fall"), 
                    "N" = c(rep("NA", 12)), 
                    "Energy.Density_Mean" = c(10.6, 9.4, 7.1, 5.9, 6.8, 4.4, 
                                              10.6, 9.4, 7.1, 5.9, 6.8, 4.4), 
                    "Energy.Density_SD" = c("NA", 1.4, "NA", 0.56, "NA", 0.82, 
                                            "NA", 1.4, "NA", 0.56, "NA", 0.82))
ed<- rbind(d, old.ed)

ed%>% 
  dplyr::mutate(Energy.Density_Mean = as.numeric(Energy.Density_Mean), 
                Energy.Density_SD = as.numeric(Energy.Density_SD), 
                upper = Energy.Density_Mean + Energy.Density_SD, 
                lower = Energy.Density_Mean - Energy.Density_SD) %>% 
  dplyr::group_by(Season, Species) %>% 
  ggplot2::ggplot(aes(x = Species, y = Energy.Density_Mean, fill = Year))+
  ggplot2::geom_bar(stat = "identity", position = "dodge")+
  ggplot2::geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                 position=position_dodge(.9)) + 
  ggplot2::facet_wrap(~Season)+
  ggplot2::ylab("Mean Energy Density")+
  ggplot2::theme(axis.title.x=element_blank(), 
                 axis.text.x = element_text(angle = 45,  hjust = 1), 
                 legend.title = element_blank())+
  ggplot2::ggtitle("Forage Fish Energy Density")+
  ecodata::theme_facet()
