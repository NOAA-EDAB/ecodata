
#hline <- mean(narw[narw$Var == "right whale abundance median",]$Value)

ecodata::narw %>% 
  dplyr::filter(Var != "Calves") %>% 
  tidyr::spread(Var, Value) %>% 
  dplyr::rename(Value = Median) %>% 
  dplyr::mutate(hline = mean(Value, na.rm = T)) %>% 
  ggplot() +
#Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = Value), size = lwd-0.75) +
  geom_point(aes(x = Time, y = Value), size = pcex-0.75) +
  geom_ribbon(aes(ymin = Lower95, ymax = Upper95, x = Time), alpha = 0.3)+
  scale_x_continuous(expand = c(0.01, 0.01)) +
  guides(color = FALSE) +
  ggtitle("NARW abundance") +
  ylab(expression("Abundance (n)")) +
  xlab("Time")+
  geom_hline(aes(yintercept = hline),
          color = "black",
          size = hline.size,
          alpha = hline.alpha,
          linetype = hline.lty) +
  theme_ts()
