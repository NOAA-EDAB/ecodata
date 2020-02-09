
prey_freq <- ecodata::common_tern %>% 
  filter(str_detect(Var, "Diet"),
         !str_detect(Var, "Sum")) %>% 
    mutate(Island = word(Var, 1),
         Var = word(Var, 4)) %>%
    group_by(Var, Time) %>% 
    dplyr::summarise(Value = sum(Value, na.rm = T)) %>% 
    group_by(Time) %>% 
    mutate(Freq = Value/sum(Value, na.rm = T)) %>% 
  ungroup()

prey_freq1 <- prey_freq %>% 
  filter(Freq > 0.05) %>% 
  dplyr::mutate(Prey = gsub("\\.", " ", Var)) %>% 
  dplyr::mutate(Prey = gsub("Other Invertebrate", "Unknown Invertebrate", Prey))

prey_freq2<- prey_freq %>% 
  filter(Freq < 0.05) %>% 
  mutate(Prey = c("<5% Occurance"))

prey_freq3<-prey_freq1 %>% 
  rbind(prey_freq2)
 colors<- c("grey", "#a6cee3", "#1f78b4", "#b2df8a", 
            "#33a02c", "#fb9a99", "#fdbf6f", 
            "#ff7f00", "#cab2d6", "#6a3d9a")

diet_freq_bar <-
  ggplot() +
  geom_bar(data = prey_freq3, 
           aes(x = Time, y = Freq, fill = Prey), 
           stat = "identity") +
  scale_x_continuous(expand = c(0.01, 0.01))+
  scale_fill_manual(values = colors) +
  ggtitle("Prey composition") +
  ylab("Proportion of prey items") +
  theme_ts()

diet_freq_bar
