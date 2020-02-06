
ecodata::zoo_abund%>%
   filter(EPU == epu_abbr) %>%
   group_by(Time) %>% 
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
   ggtitle("Zooplankton abundance anomaly") +
   scale_x_continuous(expand = c(0.01, 0.01))+
       geom_hline(aes(yintercept = hline),
            size = hline.size,
            alpha = hline.alpha,
            linetype = hline.lty)+
   theme(strip.text=element_text(hjust=0,
                                 face = "italic"))
