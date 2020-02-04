
gom_surv <-agg_bio %>% 
  filter(EPU == "GOM") 

#### Plot 1
p1<-gom_surv %>% 
  filter(str_detect(Var, "Piscivore")) %>% 
ggplot(aes(x = Time, y = Mean)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  geom_line(size = lwd-0.5) +
  geom_point(size = pcex-0.5) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  facet_wrap(Var~., ncol = 2) +
  
  #Axis and theme
  scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ylim(0, 4000)+
  ylab(expression("Biomass (kg tow"^-1*")")) +
  ggtitle("GOM NEFSC BTS") +
  theme_facet()+
  theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

#### Plot 2 
p2<-gom_surv %>% 
  filter(str_detect(Var, "Benthivore")) %>% 
ggplot(aes(x = Time, y = Mean)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  geom_line(size = lwd-0.5) +
  geom_point(size = pcex-0.5) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  ylim(0, 500)+
  #Facet 
  facet_wrap(Var~., ncol = 2) +
  
  #Axis and theme
  scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ylab(expression("Biomass (kg tow"^-1*")")) +
  theme_facet()+
  theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

## Plot 3
p3<-gom_surv %>% 
  filter(str_detect(Var, "Planktivore")) %>% 
ggplot(aes(x = Time, y = Mean)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  geom_line(size = lwd-0.5) +
  geom_point(size = pcex-0.5) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  facet_wrap(Var~., ncol = 2) +  
  ylim(0, 700)+
  #Axis and theme
  scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ylab(expression("Biomass (kg tow"^-1*")")) +
  theme_facet()+
  theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

##Plot 4
p4<-gom_surv %>% 
  filter(str_detect(Var, "Benthos")) %>% 
ggplot(aes(x = Time, y = Mean)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  geom_line(size = lwd-0.5) +
  geom_point(size = pcex-0.5) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  facet_wrap(Var~., ncol = 2) +
  ylim(0, 30)+  
  #Axis and theme
  scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  theme_facet()+
  ylab(expression("Biomass (kg tow"^-1*")")) +
  theme(strip.text=element_text(hjust=0))

plot_grid(p1, p2, p3, p4, nrow=4)
