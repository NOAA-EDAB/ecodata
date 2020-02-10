
bird<- ecodata::seabird_MAB %>% 
  mutate(hline = mean(Value))

bird %>% 
  ggplot() +
#Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = Value, color = Group), size = lwd-0.75) +
  geom_point(aes(x = Time, y = Value, color = Group), size = pcex-0.75) +
  #geom_gls(aes(x = Time, y = Value)) +
  scale_x_continuous(expand = c(0.01, 0.01),limits = c(1991,2018)) +
  ggtitle("Seabird Abundance") +
  ylab(expression("Number of Breeding Pairs")) +
  xlab("")+
  geom_hline(aes(yintercept = hline),
           color = "black",
          size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme(legend.position="bottom", legend.direction = "horizontal", 
        legend.title = element_blank(), legend.margin=margin(t = -20)) +
  theme_ts()
