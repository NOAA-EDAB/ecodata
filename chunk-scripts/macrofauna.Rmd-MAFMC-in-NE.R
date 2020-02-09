
mafmc_ne <- nefsc_survey_disaggregated %>% 
  distinct() %>% 
  filter(EPU %in% c("GOM","GB"),
         Management == "MAFMC",
         !str_detect(`Feeding guild`,"Other")) %>% 
  group_by(EPU, `Feeding guild`, Season, Time) %>% 
  dplyr::summarise(Value = sum(Proportion,na.rm = T)) %>% #IMPORTANT: Turn zeros to NA before taking mean
  mutate(Value = ifelse(Value == 0, NA, Value)) %>% #Turn zeros to NA
  unite(.,Var,c("Feeding guild","Season"), sep = " ") %>% 
  group_by(EPU,Var) %>% 
  mutate(hline = mean(Value, na.rm = T)) %>% 
  filter(str_detect(Var,"Planktivore")) %>% 
  ungroup() %>% 
  mutate(Var = paste(str_to_title(str_extract(Var, "fall|spring")),
         "survey"))
mafmc_ne$Var <- factor(mafmc_ne$Var,levels = c("Spring survey","Fall survey"))
gom_mafmc_props <- mafmc_ne %>% 
  filter(EPU == "GOM") %>% 
 ggplot(aes(x = Time, y = Value, group = Var)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Add time series
  geom_line(size = lwd-0.5) +
  geom_point(size = pcex-0.5) +
  # scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  facet_wrap(Var~.,scales = "free_y", ncol = 2) +
  ggtitle("MAFMC planktivores in Gulf of Maine")+
  #Axis and theme
  scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ylab(expression("Proportion of GOM survey")) +
  theme_facet()+
  theme(strip.text=element_text(hjust=0))

gb_mafmc_props <- mafmc_ne %>% 
  filter(EPU == "GB") %>% 
ggplot(aes(x = Time, y = Value, group = Var)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  # geom_gls(aes(x = Time, y = Value,
  #              color = Var),
  #            alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  geom_line(size = lwd-0.5) +
  geom_point(size = pcex-0.5) +
  # scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  facet_wrap(Var~.,scales = "free_y", ncol = 2) +
  ggtitle("MAFMC planktivores on Georges Bank")+
  #Axis and theme
  scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ylab("Proportion of GB survey") +
  theme_facet()+
  theme(strip.text=element_text(hjust=0))


gom_mafmc_props + gb_mafmc_props + plot_layout(ncol = 1)
