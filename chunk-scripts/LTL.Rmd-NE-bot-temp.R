
bot_temp_insitu_gom <- oceantemp_insitu %>%
  filter(EPU == "GOM",
         Var == "bottom temp anomaly in situ") %>% 
  mutate(hline = 0) %>% 
    ggplot()+ #plot
     annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = Value)) +
  geom_gls(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value), size = 1) +
  ylab("Temperature Anomaly (°C)") +
  xlab(element_blank())+
  ggtitle("GOM Bottom Temperature Anomaly") +
  scale_x_continuous(expand = c(0.01, 0.01)) +
    geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts() +
  theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))


bot_temp_insitu_gb <- oceantemp_insitu %>%
  filter(EPU == "GB",
         Var == "bottom temp anomaly in situ") %>%
   mutate(hline = 0) %>% 
    ggplot()+ #plot
     annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = Value)) +
  geom_gls(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value), size = 1) +
  ylab("Temperature Anomaly (°C)") +
  xlab(element_blank())+
  ggtitle("GB Bottom Temperature Anomaly") +
  scale_x_continuous(expand = c(0.01, 0.01)) +
    geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts() +
  theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))


bot_temp_insitu_gb +
bot_temp_insitu_gom +
  plot_layout(ncol =  1) &
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
