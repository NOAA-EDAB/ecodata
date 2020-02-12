
ecodata::ppr %>% 
  group_by(EPU) %>% 
  mutate(hline = mean(Value)) %>% 
  filter(EPU == "MAB") %>% 
  ggplot() +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_point(aes(x = Time, y = Value))+
  geom_line(aes(x = Time, y = Value))+
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  scale_x_continuous(expand = c(0.01, 0.01)) +
  ggtitle("Primary Production Required")+
  ylab("Proportion of Total PPD ")+
  theme_ts()
