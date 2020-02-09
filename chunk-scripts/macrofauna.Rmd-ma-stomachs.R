
fullness <- ecodata::stom_fullness %>%
  group_by(Var, EPU) %>% ## Remove values with missing data
  filter(n()> 10) %>% ## at least ten years of data
  ungroup() %>% 
  filter(EPU == "MAB") %>%
  ggplot(aes(x = Time, y = Value)) +
  geom_line() +
  geom_point() +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggtitle("Stomach fullness") +
  ylab("Stomach fullness") +
  facet_wrap(~Var)+
  theme(strip.text=element_text(hjust=0))

fullness
