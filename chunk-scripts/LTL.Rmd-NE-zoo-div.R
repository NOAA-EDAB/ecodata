
zoo_div <- ecodata::zoo_diversity %>% 
  filter(EPU %in% c("GOM","GB"))

gom_zoo_div <- zoo_div %>% 
  filter(EPU == "GOM") %>% 
  ggplot(aes(x = Time, y = Value, group = Var)) +
         annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls() +
  geom_line() +
  geom_point() +
  ylab("Shannon Diversity") +
  xlab(element_blank())+
  ggtitle("GOM Zooplankton Diversity") +
  facet_wrap(Var~., ncol = 3) +
  scale_x_continuous(expand = c(0.01, 0.01))+
      geom_hline(aes(yintercept = mean(Value)),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))

gb_zoo_div <- zoo_div %>% 
  filter(EPU == "GB") %>% 
  ggplot(aes(x = Time, y = Value, group = Var)) +
         annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls() +
  geom_line() +
  geom_point() +
  ylab("Shannon Diversity") +
  xlab(element_blank())+
  ggtitle("GB Zooplankton Diversity") +
  facet_wrap(Var~., ncol = 3) +
  scale_x_continuous(expand = c(0.01, 0.01))+
      geom_hline(aes(yintercept = mean(Value)),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))
  
gb_zoo_div + gom_zoo_div + plot_layout(ncol = 1)
