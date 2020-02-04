
#hline <- mean(narw[narw$Var == "right whale abundance median",]$Value)

ecodata::narw %>% 
  filter(Var != "Calves") %>% 
  ggplot() +
#Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = Value, color = Var), size = lwd-0.75) +
  #geom_point(aes(x = Time, y = Value), size = pcex-0.75) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_color_manual(values = c("red", "black", "red"))+
  guides(color = FALSE) +
  ggtitle("NARW abundance") +
  ylab(expression("Abundance (n)")) +
  xlab("Time")+
  #geom_hline(aes(yintercept = hline),
  #         color = "black",
  #         size = hline.size,
  #         alpha = hline.alpha,
  #         linetype = hline.lty) +
  theme_ts()
