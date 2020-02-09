
aggregate_prod <- ecodata::common_tern %>% 
    filter(!str_detect(Var, "Diet|Sum"))  %>% 
  mutate(Island = word(Var, 1),
         Var = word(Var, 3),
         Island = plyr::mapvalues(Island, from = c("EER","JI","MR","OGI","PINWR","SINWR","STI"),
                                  to = c("Eastern Egg Rock", "Jenny Island", "Matinicus Rock", "Outer Green Island", "Pond Island", "Seal Island","Stratton Island"))) %>%
  group_by(Time) %>% 
  dplyr::summarise(Mean = mean(Value, na.rm = T),
                   SE = sd(Value, na.rm = T)/sqrt(n()),
                   SD = sd(Value, na.rm = T),
                   n = n()) %>% 
  mutate(Mean = ifelse(is.na(SE),NA,Mean),
         se.low = Mean - SE,
         se.high = Mean + SE,
         hline = mean(Mean, na.rm = T))

aggregate_prod %>% 
  ggplot() +
#Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = Mean), size = lwd-0.75) +
  geom_point(aes(x = Time, y = Mean), size = pcex-0.75) +
  geom_gls(aes(x = Time, y = Mean)) +
  geom_errorbar(aes(x = Time,
                    ymin = se.low,
                  ymax = se.high), 
                width = 0.25) +
  scale_x_continuous(expand = c(0.01, 0.01),limits = c(1991,2019)) +
  guides(color = FALSE) +
  ggtitle("Common tern productivity") +
  ylab(expression("Fledged chicks per nest")) +
  xlab("Time")+
  geom_hline(aes(yintercept = hline),
           color = "black",
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts()
