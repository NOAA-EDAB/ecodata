
cumu <- ecodata::heatwave %>% 
  filter(Var == "cumulative intensity") %>% 
  mutate(Var = recode(Var, "cumulative intensity" = "Cumulative Intensity (degree C x days)"))

maxin <- ecodata::heatwave %>% 
  filter(Var == "maximum intensity") %>% 
  group_by(Time, EPU, Var, Units) %>% 
  summarise(Value = max(Value)) %>% 
  ungroup() %>% 
  mutate(Var = recode(Var, "maximum intensity" = "Maximum Intensity (degree C)"))

hw<- cumu %>%
  rbind(maxin) %>% 
  group_by(Var, EPU) %>% 
  mutate(hline = mean(Value))

mab.hw<- hw %>% filter(EPU == epu_abbr)
mab.hw %>% 
  ggplot() +
  geom_line(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value)) +
  geom_gls(aes(x = Time, y = Value, group = Var)) +
  ylab("") +
  xlab(element_blank())+
  ggtitle("Mid-Atlantic Marine Heatwave Intesity") +
  scale_x_continuous(expand = c(0.01, 0.01))+
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  facet_wrap(~Var, scales = "free")+
  theme_ts()+
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))
