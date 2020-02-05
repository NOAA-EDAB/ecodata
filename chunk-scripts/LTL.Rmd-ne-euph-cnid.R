
zoo_abund <- ecodata::zoo_strat_abun %>% 
  mutate(Value = Value/10^8) %>% 
  filter(EPU %in% c("GOM", "GB"),
         !str_detect(Var, "Small|Large")) %>% 
  group_by(Var, EPU) %>% 
  mutate(hline = mean(Value)) 

gom_zoo<-zoo_abund %>% 
  filter(EPU == "GOM") %>% 
  ggplot() +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls(aes(x = Time, y = Value, group = Var)) +
  geom_line(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value)) +
  ylab(expression("Stratified Abundance" (10^"8"))) +
  xlab(element_blank())+
  ggtitle("GOM Zooplankton abundance") +
  facet_wrap(Var~., ncol = 3) +
  scale_x_continuous(expand = c(0.01, 0.01))+
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))
  
gb_zoo<-zoo_abund %>% 
  filter(EPU == "GB") %>% 
  ggplot(aes(x = Time, y = Value)) +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls() +
  geom_line() +
  geom_point() +
  ylab(expression("Stratified Abundance" (10^"8"))) +
  xlab(element_blank())+
  ggtitle("GB Zooplankton abundance") +
  facet_wrap(Var~., ncol = 3) +
  scale_x_continuous(expand = c(0.01, 0.01))+
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))
  
cowplot::plot_grid(gb_zoo, gom_zoo, nrow = 2)
