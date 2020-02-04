
ecodata::cold_pool %>% 
    mutate(hline = mean(Value, na.rm = T)) %>% 
  ggplot() + 
 #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -3, ymax = 3) +
  geom_gls(aes(x = Time, y = Value),
             alpha = trend.alpha, size = trend.size) +
  geom_line(aes(x = Time, y = Value), size = lwd) +
  geom_point(aes(x = Time, y = Value), size = pcex) +
    geom_hline(aes(yintercept = hline),
     size = hline.size,
     alpha = hline.alpha,
     linetype = hline.lty)+
  ggtitle("Cold Pool Index")+
  ylab(expression("Cold Pool Temperature Anomaly (C) ")) +
  xlab("")+
  theme_ts()
