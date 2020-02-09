
aqua <- ecodata::aquaculture %>%
  group_by(Var) %>%
  mutate(hline = mean(Value)) %>%
  ungroup() %>%
  mutate(Var = plyr::mapvalues(Var, from = c("md oyster harvest","nj oyster harvest","va oyster harvest"),
                                                    to  = c("MD","NJ","VA"))) %>%
  dplyr::rename(State = Var)

aqua$State <- factor(aqua$State, levels = c("VA","MD","NJ"))


ggplot() +
  geom_segment(aes(x=2005,xend=2017,y=mean(aquaculture[aquaculture$Var == "va oyster harvest",]$Value),
                   yend=mean(aquaculture[aquaculture$Var == "va oyster harvest",]$Value)),
               size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty,
           color = "#1b9e77",
           inherit.aes = F) +
  geom_segment(aes(x=2012,xend=2016,y=mean(aquaculture[aquaculture$Var == "nj oyster harvest",]$Value),
                   yend=mean(aquaculture[aquaculture$Var == "nj oyster harvest",]$Value)),
               size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty,
           color = "#d95f02",
           inherit.aes = F) +
  geom_segment(aes(x=2012,xend=2017,y=mean(aquaculture[aquaculture$Var == "md oyster harvest",]$Value),
                   yend=mean(aquaculture[aquaculture$Var == "md oyster harvest",]$Value)),
               size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty,
           color = "#7570b3",
           inherit.aes = F) +
 #Highlight last ten years
  geom_line(data = aqua, aes(x = Time, y = Value, color = State), size = lwd) +
  geom_point(data = aqua,aes(x = Time, y = Value, color = State), size = pcex) +
  scale_color_manual(values = c(VA = "#1b9e77", MD = "#7570b3",NJ = "#d95f02")) +
  scale_x_continuous(breaks = seq(2005,2018,3),expand = c(0.01, 0.01)) +
  scale_y_continuous(labels = function(l){trans = l / 1000000})+
  ggtitle("Oyster harvest")+
  ylab(expression("Oysters sold (10"^6*" n)")) +
  xlab("")+
  theme_ts()
