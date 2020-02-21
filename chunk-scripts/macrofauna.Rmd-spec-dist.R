
spec_dist <- ecodata::species_dist %>% 
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value))

asd <- spec_dist %>% 
  dplyr::filter(Var == "along-shelf distance") %>% 
  ggplot(aes(x = Time, y = Value,
               group = Var)) + 
 #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls() +
  geom_line() +
  geom_point() +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  ggtitle("Along-shelf distance")+
  ylab(expression("Distance (km)")) +
  xlab("")+
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts() 

depth <- spec_dist %>% 
  filter(Var == "depth") %>% 
  mutate(Value = Value* -1, 
         hline = mean(Value)) %>% 
  ggplot(aes(x = Time, y = Value,
               group = Var)) + 
 #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls() +
  geom_line() +
  geom_point() +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  ggtitle("Depth") +
  ylab(expression("Depth (m)")) +
  xlab("")+
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts() 

dtc <- spec_dist %>% 
  filter(Var == "distance to coast") %>% 
  ggplot(aes(x = Time, y = Value,
               group = Var)) + 
 #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls() +
  geom_line() +
  geom_point() +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  ggtitle("Distance to coast")+
  ylab(expression("Distance (km)")) +
  xlab("Time")+
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts() 

asd + depth + plot_layout(ncol = 1) 
