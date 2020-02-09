
GB_prop <- 
  prop %>% 
    filter(EPU == "GB") %>% 
ggplot(aes(x = Time, y = Proportion, color = Management, group = Var2)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
           xmin = x.shade.min , xmax = x.shade.max ,
           ymin = -Inf, ymax = Inf) +
  
  #Add time series
  geom_line(size = 0.5) +
  geom_point_interactive(aes(tooltip = tt),size = 0.5) +
#  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 group = Var2,
                 color = Management),
             size = 0.5,
             alpha = hline.alpha,
             linetype = hline.lty)+
  
  #Facet 
  facet_wrap(Var~., ncol = 2) +
  
  #Axis and theme
  scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ylab(expression("Proportion of survey")) +
  ggtitle("GB") +
  theme_facet()+
  theme(strip.text=element_text(hjust=0))
GB_prop <- girafe(ggobj = GB_prop)
GB_prop <- girafe_options(GB_prop, opts_zoom(max = 5) )
GB_prop
