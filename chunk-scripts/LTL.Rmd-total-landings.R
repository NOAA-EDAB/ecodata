
total_landings_agg <- total_landings %>%
  group_by(Time) %>%
  dplyr::summarise(Value = sum(Value)) %>% 
  mutate(Var = "Total",hline = mean(Value))
managed_landings_agg <- managed_landings %>%
  group_by(Time) %>%
  dplyr::summarise(Value = sum(Value)) %>% 
  mutate(Var = "Managed",hline = mean(Value))

landings_agg <- rbind(total_landings_agg, managed_landings_agg)


ggplot(data = landings_agg)+
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls(aes(x = Time, y = Value,
               group = Var),
             alpha = trend.alpha, size = trend.size) +
  geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
  geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +

  scale_y_continuous(labels = function(l){trans = l / 1000})+
  scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  ylab(expression("Landings (10"^3*"metric tons)")) +

  geom_hline(aes(yintercept = hline,
               
               color = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
    theme_ts()
