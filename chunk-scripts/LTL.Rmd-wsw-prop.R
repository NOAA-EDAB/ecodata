
sw.df <- slopewater %>% 
  mutate(Var, Var = plyr::mapvalues(Var, from = c("WSW proportion ne channel",
                                                  "LSLW proportion ne channel"),
                                    to = c("WSW","LSLW"))) %>% 
  dplyr::rename(Flavor  = Var) %>% 
  group_by(Flavor) %>% 
  mutate(hline = mean(Value)) 

sw.df$Flavor <- factor(sw.df$Flavor, levels = c("WSW","LSLW"))

ggplot(data = sw.df) +
  geom_line(aes(x = Time, y = Value, color = Flavor))+
  geom_point(aes(x = Time, y = Value, color = Flavor)) +
  ylab("Percent of Total Slopewater") +
  xlab(element_blank())+
  ggtitle("Slopewater Proportions in NE Channel")+
    scale_x_continuous(expand = c(0.01, 0.01))+
      geom_hline(aes(yintercept = hline,
                     color = Flavor),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_ts() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))
