
zoo_abund <- ecodata::zoo_strat_abun %>% 
  mutate(Value = Value/10^8) %>% 
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
  scale_color_manual(values = c("#ca0020", "black"))+
  geom_gls(aes(x = Time, y = Value, group = Var)) +
  ylab(expression("Stratified Abundance" (10^"8"))) +
  xlab(element_blank())+
  ggtitle("Small and large calanoid abundance") +
  facet_wrap(EPU~., ncol = 1, scales = "free") +
  scale_x_continuous(expand = c(0.01, 0.01))+
  geom_hline(aes(yintercept = hline,
           color = Var), 
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"), 
        legend.position = "none")
