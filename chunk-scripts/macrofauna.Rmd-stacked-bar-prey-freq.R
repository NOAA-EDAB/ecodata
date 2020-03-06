
prey_freq <- ecodata::seabird_ne %>% 
  dplyr::filter(stringr::str_detect(Var, "Diet"),
         !stringr::str_detect(Var, "Sum")) %>% 
  dplyr::mutate(Island = word(Var, 1),
         Var = word(Var, 4)) %>%
  dplyr::group_by(Var, Time) %>% 
  dplyr::summarise(Value = sum(Value, na.rm = T)) %>% 
  dplyr::group_by(Time) %>% 
  dplyr::mutate(Freq = Value/sum(Value, na.rm = T)) %>% 
  dplyr::ungroup()

prey_freq1 <- prey_freq %>% 
  dplyr::filter(Freq > 0.05) %>% 
  dplyr::mutate(Prey = gsub("\\.", " ", Var)) %>% 
  dplyr::mutate(Prey = gsub("Other Invertebrate", "Unknown Invertebrate", Prey))

prey_freq2<- prey_freq %>% 
  dplyr::filter(Freq < 0.05) %>% 
  dplyr::mutate(Prey = c("<5% Occurance"))

prey_freq3<-prey_freq1 %>% 
  rbind(prey_freq2)
colors<- c("grey", "#a6cee3", "#1f78b4", "#b2df8a", 
            "#33a02c", "#fb9a99", "#fdbf6f", 
            "#ff7f00", "#cab2d6", "#6a3d9a")

diet_freq_bar <-
  ggplot2::ggplot() +
  ggplot2::geom_bar(data = prey_freq3, 
           aes(x = Time, y = Freq, fill = Prey), 
           stat = "identity") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Prey composition") +
  ggplot2::ylab("Proportion of prey items") +
  ecodata::theme_ts()

diet_freq_bar
