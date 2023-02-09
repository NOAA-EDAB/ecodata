
ecodata::heatwave_year %>% 
  filter(EPU == "GOM", 
         str_detect(t, "2022"), 
         Var == "BottomDetrended") %>% 
  ggplot( aes(x = t, y = temp))+
  geom_flame(aes(y2 = thresh))+ 
  geom_line(aes(x = t, y = seas, color = "a"), size = 1)+
  geom_line(aes(x = t, y = thresh, color = "c"), size = 1)+
  geom_line(aes(x = t, y = temp, color = "b"))+
  scale_colour_manual(values = c("turquoise4", "sienna3", "black"),
                      labels = c("Climatology","Temperature", "Threshold"))+
  ylab("Temperature (C)")+
  xlab(element_blank())+
  scale_x_date(date_labels = "%b", breaks = "1 month")+
  theme_bw()+
  ggplot2::ggtitle("Gulf of Maine - Bottom")+
  theme(legend.title = element_blank(),
        legend.position=c(0.3, 0.8))+
  ecodata::theme_title()
