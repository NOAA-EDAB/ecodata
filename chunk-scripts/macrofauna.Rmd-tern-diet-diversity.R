
diet_div <- ecodata::common_tern %>% 
  dplyr::filter(str_detect(Var, "Diet"),
         !str_detect(Var, "Sum")) %>% 
  dplyr::mutate(Island = word(Var, 1),
         Var = word(Var, 4)) %>% 
  dplyr::group_by(Island, Time) %>%
  dplyr::summarise(evenness = diversity(Value)/log(specnumber(Value)),
                   shannon = diversity(Value),
                   simpson = diversity(Value, index = "simpson")) %>% 
  tidyr::gather(.,Var,Value,-Island, -Time) %>% 
  dplyr::group_by(Var, Time) %>%
  dplyr::summarize(Value = mean(Value, na.rm = T),
                   sd = sd(Value, na.rm = T),
                   n = n()) %>%
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value, na.rm = T))


shannon <- diet_div %>% 
  dplyr::filter(Var == "shannon") %>% 
  ggplot(aes(x = Time, y = Value)) +
      annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line() +
  geom_point() +
  #geom_gls() +
  scale_x_continuous(expand = c(0.01, 0.01),limits = c(1992,2018)) +
  ggtitle("Common tern diet diversity")+
  ylab(expression("Shannon Diversity")) +
  xlab("")+
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts() 

shannon 
