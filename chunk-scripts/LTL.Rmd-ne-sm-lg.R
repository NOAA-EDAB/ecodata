
zoo_abund <- ecodata::zoo_strat_abun %>% 
  filter(EPU %in% c("GOM", "GB"),
         str_detect(Var, "Small|Large")) %>% 
  group_by(Var, EPU) %>% 
  mutate(hline = mean(Value)) 

zoo_abund %>% 
  ggplot() +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = Value, color = Var)) +
  geom_point(aes(x = Time, y = Value, color = Var)) +
  geom_gls(aes(x = Time, y = Value, group = Var)) +
  ylab("Abundance Estimate (n)") +
  xlab(element_blank())+
  ggtitle("Zooplankton abundance") +
  facet_wrap(EPU~., ncol = 1, scales = "free") +
  scale_x_continuous(expand = c(0.01, 0.01))+
  geom_hline(aes(yintercept = hline,
           color = Var), 
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))
