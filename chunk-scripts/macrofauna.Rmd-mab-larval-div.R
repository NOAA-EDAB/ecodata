
ma_larv_div <- ecodata::ichthyo_diversity %>%
  filter(EPU == "MAB",
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
  ggtitle("Mid-Atlantic larval diversity") +
  ylab("Shannon Diversity") +
  facet_wrap(Var~., ncol = 2, scales = "free_y") +
  scale_x_continuous(expand = c(0.01, 0.01))+
      geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0))

ma_larv_div
