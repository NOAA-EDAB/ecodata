
d<-ecodata::energy_density

old.ed<- data.frame("Species" = c("Alewife", "Atl. Herring","Atl. Mackerel","Butterfish", "Illex squid", "Loligo squid",  "Sand lance","Silver hake" ,  "Atl. Herring", "Illex squid","Sand lance"),
                    "Year" = c("1980s", "1980s", "1980s", "1980s", "1980s", "1980s","1980s", "1980s", "1990s",
                               "1990s","1990s"), 
                    "Season" = c("All", "All", "All", "All", "All", "All", "All", "All" , "All", "All", "All"), 
                    "N" = c(rep("NA", 11)), 
                    "Energy.Density_Mean" = c(6.4, 10.6, 6.0, 6.2, 7.1, 5.6, 6.8, 4.6, 9.4, 5.9, 4.4))


d %>% 
  dplyr::mutate(Energy.Density_Mean = as.numeric(Energy.Density_Mean), 
                Energy.Density_SD = as.numeric(Energy.Density_SD), 
                upper = Energy.Density_Mean + Energy.Density_SD, 
                lower = Energy.Density_Mean - Energy.Density_SD) %>% 
  dplyr::group_by(Season, Species) %>% 
  ggplot2::ggplot(aes(x=Year, y = Energy.Density_Mean, color = Season))+
  ggplot2::geom_point()+
  geom_line()+
  ggplot2::geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) + 
  geom_hline(data = old.ed, aes(yintercept = Energy.Density_Mean, linetype =Year))+
  ggplot2::facet_wrap(~Species, nrow = 2)+
  ggplot2::ylab("Mean Energy Density (kJ/g)")+
  ggplot2::theme(axis.title.x=element_blank(), 
                 axis.text.x = element_text(angle = 45,  hjust = 1), 
                 legend.title = element_blank())+
  scale_x_continuous(breaks=c(2017,2018, 2019))+
  ggplot2::ggtitle("Forage Fish Energy Density")+
  ecodata::theme_facet()
