
temp_anom <- ecodata::oceantemp_insitu %>% 
  filter(EPU == epu_abbr) %>% 
  complete(Time = full_seq(min(oceantemp_insitu$Time):max(oceantemp_insitu$Time),1),
           nesting(Var)) %>% 
  mutate(hline = 0)

temp_anom %>%
 filter(Var == "bottom temp anomaly in situ") %>%
ggplot2::ggplot() +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = Value)) +
  geom_gls(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value), size = 1) +
  ylab("Temperature (°C)") +
  xlab(element_blank())+
  ggtitle("Bottom temp. anomaly") +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  theme_ts() +
  theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))+
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
 theme_ts()
