
eng %>% filter(EPU == "GOM") %>% 
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
