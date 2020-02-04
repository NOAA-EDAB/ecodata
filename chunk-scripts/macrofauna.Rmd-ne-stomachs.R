
gb_fullness <- ecodata::stom_fullness %>%
  group_by(Var, EPU) %>% ## Remove values with missing data
  filter(n()> 10) %>% ## at least tens years
  ungroup() %>% 
  filter(EPU == "GB") %>%
  ggplot(aes(x = Time, y = Value)) +
  geom_line() +
  #geom_point() +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggtitle("GB Stomach fullness") +
  ylab("Stomach fullness anomaly") +
  facet_wrap(~Var)+
  theme(strip.text=element_text(hjust=0), 
        legend.position = "none", 
        axis.text.x = element_text(angle = 45))

gom_fullness <- ecodata::stom_fullness %>%
  group_by(Var, EPU) %>% ## Remove values with missing data
  filter(n()> 10) %>% ## at least tens years
  ungroup() %>% 
  filter(EPU == "GOM") %>%
  ggplot(aes(x = Time, y = Value)) +
  geom_line() +
  #geom_point() +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggtitle("GOM Stomach fullness") +
  ylab("Stomach fullness anomaly") +
  facet_wrap(~Var)+
  theme(strip.text=element_text(hjust=0), 
        legend.position = "none", 
        axis.text.x = element_text(angle = 45))

gb_fullness
gom_fullness
