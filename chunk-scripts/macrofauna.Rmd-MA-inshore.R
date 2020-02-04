
mass <- mass_inshore_survey %>% 
  filter(EPU == "GB",
         Time>1980) %>% 
  separate(Var, c("feeding.guild", "season", "biomass", "stat")) %>% 
  unite(.,Var, c("feeding.guild","season"),sep = " ") %>% 
  group_by(Time, Var) %>% 
  spread(stat, Value) %>% 
  mutate(upper = Index + SD, 
         lower = Index - SD) %>% 
  group_by(Var) %>% 
  mutate(hline = mean(Index))
mass$Var <- factor(mass$Var,levels = c("Piscivore Spring","Piscivore Fall",
                                       "Benthivore Spring","Benthivore Fall",
                                       "Planktivore Spring","Planktivore Fall",
                                       "Benthos Spring","Benthos Fall")) 
###Plot 1
p1<-mass %>% 
  filter(!is.na(Var), 
         str_detect(Var, "Piscivore")) %>% 
  ggplot(aes(x = Time, y = Index)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Index,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
    geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  geom_line(size = lwd-0.5) +
  geom_point(size = pcex-0.5) +
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
  ylab(expression("Biomass (kg tow"^-1*")")) +
  ggtitle("MA Inshore BTS") +
  theme_facet()+
  theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

###Plot 2
p2<-mass %>% 
  filter(!is.na(Var), 
         str_detect(Var, "Benthivore")) %>% 
  ggplot(aes(x = Time, y = Index)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Index,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  geom_line(size = lwd-0.5) +
  geom_point(size = pcex-0.5) +
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
  ylab(expression("Biomass (kg tow"^-1*")")) +
  theme_facet()+
  theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

###Plot 3
p3<-mass %>% 
  filter(!is.na(Var), 
         str_detect(Var, "Planktivore")) %>% 
  ggplot(aes(x = Time, y = Index)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Index,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  geom_line(size = lwd-0.5) +
  geom_point(size = pcex-0.5) +
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
  ylab(expression("Biomass (kg tow"^-1*")")) +
  theme_facet()+
  theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

###Plot 4
p4<-mass %>% 
  filter(!is.na(Var), 
         str_detect(Var, "Benthos")) %>% 
  ggplot(aes(x = Time, y = Index)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Index,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  geom_line(size = lwd-0.5) +
  geom_point(size = pcex-0.5) +
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
  ylab(expression("Biomass (kg tow"^-1*")")) +
  theme_facet()+
  theme(strip.text=element_text(hjust=0))

plot_grid(p1, p2, p3, p4, nrow = 4)
