
agg<-ecodata::agg_bio %>% 
  filter(!str_detect(Var, "Apex|inshore|offshore|managed|NEFMC|MAFMC|JOINT|NA")) %>% #remove unused datasets
  separate(Var, c("feeding.guild", "season", "Biomass", "Var1"), sep = " ") %>% 
  unite("Var", feeding.guild:season, sep = " ") %>% 
  mutate(stat = recode(Var1, Index = "Mean", 
                      Standard = "SD")) %>% 
  dplyr::select(-Biomass, -Var1) %>% 
  group_by(Var, Time, EPU) %>% 
  spread(stat, Value) %>% 
  mutate(upper = Mean + SD, 
         lower = Mean - SD)


agg_bio<-agg %>% filter(EPU == epu_abbr,
         Time >= 1968) %>% 
  group_by(Var, EPU) %>% 
  mutate(hline = mean(Mean, na.rm = T)) %>% 
  ungroup() 

agg_bio$Var <- factor(agg_bio$Var,levels = c("Piscivore Spring",
                                                   "Piscivore Fall",
                                                    "Benthivore Spring",
                                                   "Benthivore Fall",
                                                    "Planktivore Spring",
                                                    "Planktivore Fall",
                                                    "Benthos Spring",
                                                   "Benthos Fall"))
series.col <- rep("black",length(unique(agg_bio$Var)))
facet_names <- list("Piscivores" = expression("Piscivores"),
                    "Planktivores" = expression("Planktivores"),
                    "Benthivores" = expression("Benthivores"),
                    "Benthos" = expression("Benthos"))
#Get NEAMAP
neamap <- ecodata::mab_inshore_survey %>% 
  group_by(Var) %>% 
  mutate(hline = mean(Value),
         SD = Value * CV, #calculate SD from CV
         upper = Value + SD, 
         lower = Value - SD)

neamap$Var <- factor(neamap$Var,levels = c("Piscivore Spring","Piscivore Fall",
                                           "Benthivore Spring", "Benthivore Fall",
                                           "Planktivore Spring", "Planktivore Fall",  
                                           "Benthos Spring", "Benthos Fall"))
## Piscivore 
neamap.1<-neamap %>% 
  filter(str_detect(Var,"Piscivore"))
p1<-agg_bio %>% 
  filter(str_detect(Var,"Piscivore")) %>% 
  ggplot() +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +

  #Add time series
  geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") +
  geom_line(aes(x = Time, y = Mean),size = lwd-0.5) +
  geom_point(aes(x = Time, y = Mean),size = pcex-0.5) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
  facet_wrap(Var~.,ncol = 2) +
     #Add NEAMAP
    geom_ribbon(data = neamap.1, aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "pink")+
  geom_line(data = neamap.1, aes(x = Time, y = Value),
            color = "#ca0020")+
  geom_point(data = neamap.1, aes(x = Time, y = Value),
             size = pcex-0.5,
             color = "#ca0020")+

  #Axis and theme
  scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  #ylim(0, 1200)+
  ylab(expression("Biomass (kg tow"^-1*")")) +
  theme_facet()+
  theme(strip.text=element_text(hjust=0), 
        axis.title.x=element_blank())

## Benthivore
neamap.2<-neamap %>% 
  filter(str_detect(Var,"Benthivore"))
p2<-agg_bio %>% 
  filter(str_detect(Var,"Benthivore")) %>% 
  ggplot() +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  geom_ribbon( aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") +
  geom_line(aes(x = Time, y = Mean),size = lwd-0.5) +
  geom_point(aes(x = Time, y = Mean),size = pcex-0.5) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
  facet_wrap(Var~.,ncol = 2) +
       #Add NEAMAP
  geom_ribbon(data = neamap.2, aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5, fill = "pink")+
  geom_line(data = neamap.2, aes(x = Time, y = Value),
            color = "#ca0020")+
  geom_point(data = neamap.2, aes(x = Time, y = Value),
             size = pcex-0.5,
             color = "#ca0020")+

  #Axis and theme
  scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ylab(expression("Biomass (kg tow"^-1*")")) +
  theme_facet()+
  theme(strip.text=element_text(hjust=0), 
        axis.title.x=element_blank())


### Planktivore
neamap.3<-neamap %>% 
  filter(str_detect(Var,"Planktivore"))
p3<-agg_bio %>% 
  filter(str_detect(Var,"Planktivore")) %>% 
  ggplot() +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") +
  geom_line(aes(x = Time, y = Mean),size = lwd-0.5) +
  geom_point(aes(x = Time, y = Mean),size = pcex-0.5) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
  facet_wrap(Var~.,ncol = 2) +
       #Add NEAMAP
    geom_ribbon(data = neamap.3, aes(ymax = pmax(upper, 0), ymin = lower, x = Time), 
                fill = "pink", alpha = 0.5) +
  geom_line(data = neamap.3, aes(x = Time, y = Value),
            color = "#ca0020")+
  geom_point(data = neamap.3, aes(x = Time, y = Value),
             size = pcex-0.5,
             color = "#ca0020")+

  #Axis and theme
  scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  #ylim(0, 600)+
  ylab(expression("Biomass (kg tow"^-1*")")) +
  theme_facet()+
  theme(strip.text=element_text(hjust=0), 
        axis.title.x=element_blank())

### Benthos
neamap.4<-neamap %>% 
  filter(str_detect(Var,"Benthos"))
p4<-agg_bio %>% 
  filter(str_detect(Var,"Benthos")) %>% 
  #ggplot(aes(x = Time, y = Mean)) +
  ggplot() +
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  #Add time series
  geom_ribbon( aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  geom_line(aes(x = Time, y = Mean),size = lwd-0.5) +
  geom_point(aes(x = Time, y = Mean), size = pcex-0.5) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
  facet_wrap(Var~.,ncol = 2) +
       #Add NEAMAP
  geom_ribbon(data = neamap.4, aes(ymax = pmax(upper, 0), ymin = lower, x = Time),
              fill = "pink", alpha = 0.5) +
  geom_line(data = neamap.4, aes(x = Time, y = Value),
            color = "#ca0020")+
  geom_point(data = neamap.4, aes(x = Time, y = Value),
             size = pcex-0.5,
             color = "#ca0020")+

  scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +

  ylab(expression("Biomass (kg tow"^-1*")")) +
  theme_facet()+
  theme(strip.text=element_text(hjust=0), 
        axis.title.x=element_blank())
plot_grid(p1, p2, p3, p4, nrow=4)
