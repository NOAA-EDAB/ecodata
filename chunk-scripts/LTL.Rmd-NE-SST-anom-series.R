
ne_anom <- seasonal_oisst_anom %>% 
  filter(EPU %in% c("GB","GOM")) %>% 
    mutate(hline = 0,
           Var = str_to_title(str_extract(Var,"winter|spring|summer|fall")))
ne_anom$Var <- factor(ne_anom$Var, levels= c("Winter","Spring","Summer","Fall"))

ne_anom_plt <- ggplot(data = ne_anom, 
       aes(x = Time, y = Value, color = EPU, group = EPU))+
     annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line()+
  geom_point()+
  ylim(-2,3)+
  geom_gls() +
  ylab(expression("SST Anomaly (C)")) +
  xlab(element_blank())+
  ggtitle("Gulf of Maine & Georges Bank SST Anomaly") +
    scale_color_manual(values = c("black","indianred"))+
    scale_x_continuous(expand = c(0.01, 0.01)) +
    geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  facet_wrap(Var ~., ncol = 2, scales = "free_y")+
  theme_facet() +
  theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))
ne_anom_plt
