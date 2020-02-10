
eng<-ecodata::engagement %>% 
  filter(!Var == "med.high.scores")
eng$Var <- factor(eng$Var, levels = c("%High","%Medium High","%Moderate", "%Low"))
eng %>% filter(EPU == "NE") %>% 
  ggplot()+
   #ylim(0.8, NA)+
  geom_bar(aes(x = Time, y = Value, 
               fill = Var), 
           stat = "identity")+
  #scale_y_continuous(labels = Value(suffix = "%", prefix = "")) +
  #geom_text(aes(x = Time, y = Value,
  #             label = paste0(Value,"%")), size=4) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank())+
  coord_cartesian(ylim=c(0.75,1))+
  xlab("Time") +
  ylab("% Communities in each category (Low to High)")+
  ggtitle("Commercial Engagement")+
  theme_ts()


ecodata::engagement %>% 
  filter(Var == "med.high.scores", 
         EPU == "NE") %>% 
  mutate(hline = mean(Value)) %>% 
  ggplot()+
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = Value), size = lwd) +
  geom_point(aes(x = Time, y = Value), size = pcex) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  ggtitle("Medium-High communities ") +
  ylab(expression("Average score for Med High communities")) +
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
 theme_ts()
