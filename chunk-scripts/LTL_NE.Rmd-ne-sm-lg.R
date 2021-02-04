
ecodata::zoo_sli_anom %>%
  filter(EPU %in% c("GOM","GB")) %>%
  mutate(hline = 0) %>% 
  ggplot(aes(x = Time, y = Value, color = Var)) +
         annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  #geom_gls() +
  geom_line() +
  geom_point() +
  ylab("Abundance anomaly") +
  xlab(element_blank())+
  ggtitle("Small and large-bodied copepod abundance anomaly") +
  facet_wrap(EPU~., nrow = 2) +
  scale_x_continuous(expand = c(0.01, 0.01))+
  scale_colour_discrete(name = "Copeopds", labels = c("large-bodied", "small-bodied"))+
      geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"), 
        legend.title = element_blank()) 
