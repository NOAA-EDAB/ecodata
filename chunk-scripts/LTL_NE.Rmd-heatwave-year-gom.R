
ecodata::heatwave_year %>% 
  filter(EPU == "GOM", 
         Year == "2020") %>% 
  ggplot( aes(x = t, y = temp))+
  geom_flame(aes(y2 = thresh))+ 
  geom_line(aes(x = t, y = seas, color = "a"), size = 1)+
  geom_line(aes(x = t, y = thresh, color = "c"), size = 1)+
  geom_line(aes(x = t, y = temp, color = "b"))+
  scale_colour_manual(values = c("turquoise4", "sienna3", "black"),
                      labels = c("Climatology","Temperature", "Threshold"))+
  ylab("Temperature (C)")+
  xlab(element_blank())+
  ggtitle("GOM Marine Heatwaves 2020")+
  scale_x_date(date_labels = "%b", breaks = "1 month")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position=c(0.2, 0.8))+
  ecodata::theme_title()
