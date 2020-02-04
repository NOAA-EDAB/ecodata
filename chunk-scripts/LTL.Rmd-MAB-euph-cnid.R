
zoo_abund <- ecodata::zoo_strat_abun %>% 
  filter(EPU == epu_abbr,
         !str_detect(Var, "Small|Large")) %>% 
  group_by(Var, EPU) %>% 
  mutate(hline = mean(Value)) 

zoo_abund %>% 
  ggplot(aes(x = Time, y = Value)) +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls() +
  geom_line() +
  geom_point() +
  ylab("Stratified Abundance") +
  xlab(element_blank())+
  ggtitle("Zooplankton abundance") +
  facet_wrap(Var~., ncol = 3) +
  scale_x_continuous(expand = c(0.01, 0.01))+
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))
