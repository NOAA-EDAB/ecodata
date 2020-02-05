
gb.hw<-hw %>% filter(EPU == "GB") %>% 
  ggplot() +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value)) +
  geom_gls(aes(x = Time, y = Value)) +
  ylab("") +
  xlab(element_blank())+
  ggtitle("Georges Bank") +
  scale_x_continuous(expand = c(0.01, 0.01))+
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  
  facet_wrap(~Var, scales = "free") +
  theme_ts()+
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))


gom.hw<-hw %>% filter(EPU == "GOM") %>% 
  ggplot() +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value)) +
  geom_gls(aes(x = Time, y = Value)) +
  ylab("") +
  xlab(element_blank())+
  ggtitle("Gulf of Maine") +
  scale_x_continuous(expand = c(0.01, 0.01))+
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  facet_wrap(~Var, scales = "free") +
  theme_ts()+
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))
cowplot::plot_grid(gb.hw,gom.hw, nrow = 2)
