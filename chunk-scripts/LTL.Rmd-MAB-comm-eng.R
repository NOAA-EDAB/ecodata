
eng<-ecodata::eng_rel
eng$State <- sub(".*\\s+", "", eng[,1])

eng<-eng %>% 
  mutate(score= case_when(
    (Value > 1) ~ "a",  
    (Value < 1) ~ "b")) %>% 
  group_by(Time, EPU) %>% 
  summarise(propa = trunc(length(which(score == "a"))/(length(score))*100), 
            propb = 100 - propa) %>% 
  gather(., "engagement.proportion", "val", 3:4)
  
eng$engagement.proportion <-  factor(eng$engagement.proportion, levels = c("propb","propa"),
                              labels = c("Proportion of communites <1","Propotion of ocmmunities >1"))
eng<- eng %>% 
  group_by(Time, EPU) %>% 
  mutate(pos = cumsum(val) - (0.5 * val))

eng %>% filter(EPU == "MAB") %>% 
  ggplot()+
    geom_bar(aes(x = Time, y = val, 
                 fill = engagement.proportion), 
             stat = "identity")+
  #scale_y_continuous(labels = val(suffix = "%", prefix = "")) +
  geom_text(aes(x = Time, y = pos,
                label = paste0(val,"%")), size=4) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank())+
    xlab("Time") +
    ylab("Commercial engagement")+
    ggtitle("Commercial Engagement")+
    theme_ts()
