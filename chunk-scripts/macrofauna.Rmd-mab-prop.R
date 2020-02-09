
prop <- ecodata::nefsc_survey_disaggregated %>% 
  filter(!str_detect(`Feeding guild`, "Other|other"),
         !is.na(Proportion)) %>% #remove "other"
  unite(.,Var,c("Feeding guild","Season"), sep = " ") %>% 
  filter(!str_detect(Var,"Apex|Benthos"),
         !is.na(Management)) %>% #remove benthos and apex predators
    group_by(EPU, Management, Var, Time) %>%
  dplyr::summarise(Proportion = sum(Proportion)) %>% 
  mutate(Var2 = paste(Management,Var),
         tt = paste(Management,round(Proportion,2), Time),
         Proportion = ifelse(Proportion == 0, NA, Proportion)) %>% #tt = tooltip
    group_by(EPU,Var2) %>% 
  mutate(hline = mean(Proportion,na.rm = T)) %>% 
    complete(Time = full_seq(min(.$Time):max(.$Time),1),
           nesting(Management, Var)) 


#Change factor levels
prop$Var <- factor(prop$Var,levels = c("Piscivore spring","Piscivore fall",
                                       "Benthivore spring","Benthivore fall",            
                                       "Planktivore spring", "Planktivore fall" ))

mab_prop <- 
  prop %>% 
    filter(EPU == "MAB") %>% 
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
  ggtitle("MAB") +
  theme_facet()+
  theme(strip.text=element_text(hjust=0))
mab_prop <- girafe(ggobj = mab_prop)
mab_prop <- girafe_options(mab_prop, opts_zoom(max = 5) )
mab_prop
