
gom_larv_div <- ecodata::ichthyo_diversity %>%
  filter(EPU == "GOM",
         str_detect(Var, "Ich_Shannon")) %>%
  mutate(Var = word(Var,1)) %>% 
  group_by(Var) %>% 
  mutate(hline = mean(Value, na.rm = T)) %>% 
  ggplot(aes(x = Time, y = Value, group = Var)) +
  geom_line() +
  geom_point() +
       annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls(aes(x = Time, y = mean(Value))) +
  ggtitle("GOM larval diversity") +
  ylab("Shannon Diversity") +
  scale_x_continuous(expand = c(0.01, 0.01))+
      geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0))

gb_larv_div <- ecodata::ichthyo_diversity %>%
  filter(EPU == "GB",
         str_detect(Var, "Ich_Shannon")) %>%
  mutate(Var = word(Var,1)) %>% 
  group_by(Var) %>% 
  mutate(hline = mean(Value, na.rm = T)) %>% 
  ggplot(aes(x = Time, y = Value, group = Var)) +
  geom_line() +
  geom_point() +
       annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls(aes(x = Time, y = mean(Value))) +
  ggtitle("GB larval diversity") +
  ylab("Shannon Diversity") +
  scale_x_continuous(expand = c(0.01, 0.01))+
      geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0))

gb_larv_div + gom_larv_div + plot_layout(ncol = 1)
