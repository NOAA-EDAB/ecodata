
upper.line<-ecodata::wcr %>%
  filter(Time>2000) %>% 
  mutate(hline = c(mean(Value)))
lower.line<-ecodata::wcr%>%
  filter(Time<2000) %>% 
  mutate(hline = c(mean(Value)))
wcr<- upper.line %>% 
  rbind(lower.line)

wcr %>% 
  ggplot(aes(x = Time, y = Value))+
  geom_point()+
  geom_line()+
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ylab("Warm Core Ring Births")+
  xlab(element_blank())+
  ggtitle("Warm Core Rings")+
  theme_ts()+
  geom_segment(data = upper.line, aes(x = min(Time), y = hline, 
                                      xend = max(Time), yend = hline, color = "segment") )+
  geom_segment(data = lower.line, aes(x = min(Time), y = hline, 
                                      xend = max(Time), yend = hline, color = "segment") )+
  theme(legend.position = "none")
