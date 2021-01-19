
df12<-ecodata::heatwave_year %>% 
  filter(EPU == "GB", 
         Year == "2012")

ecodata::heatwave_year %>% 
  filter(EPU == "GB", 
         Year == "2020") %>% 
  ggplot( aes(x = t, y = temp))+
  #geom_flame(aes(y = temp, y2 = thresh, fill = "sienna3"), show.legend = T) +
  #geom_flame(data = df12, aes(y = temp, y2 = thresh, fill = "red"),  show.legend = T) +
  heatwaveR::geom_flame(aes(y2 = thresh))+ 
  #heatwaveR::geom_flame(aes(y2 = df12$thresh))+ 
  geom_line(aes(x = t, y = seas, color = "a"), size = 1)+
  geom_line(aes(x = t, y = thresh, color = "c"), size = 1)+
  geom_line(aes(x = t, y = temp, color = "b"))+
  #geom_line(aes(x = df12$doy, y = df12$temp, color = "d"))+
  scale_colour_manual(values = c("turquoise4",   "sienna3","black"),
                      labels = c("Climatology","Temperature","Threshold"))+
  ylab("Temperature (°C)")+
  xlab(element_blank())+
  scale_x_date(date_labels = "%b", breaks = "1 month")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position=c(0.2, 0.8))
