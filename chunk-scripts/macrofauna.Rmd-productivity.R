
ecodata::common_tern %>% 
    filter(!str_detect(Var, "Diet|Sum"))  %>% 
  mutate(Island = word(Var, 1),
         Var = word(Var, 3),
         Island = plyr::mapvalues(Island, from = c("EER","JI","MR","OGI","PINWR","SINWR","STI"),
                                  to = c("Eastern Egg Rock", "Jenny Island", "Matinicus Rock", "Outer Green Island", "Pond Island", "Seal Island","Stratton Island"))) %>%
  group_by(Island) %>% 
  mutate(hline = mean(Value, na.rm = T)) %>% 
ggplot(aes(x = Time, y = Value, group = Island)) +
      annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line() +
  geom_point() +
  guides(color = F)+
  scale_x_continuous(expand = c(0.01, 0.01)) +
  facet_wrap(Island~., scales = "free_y", nrow = 2)+
  ggtitle("Productivity")+
  ylab(expression("Fledged chicks per nest")) +
  xlab("Time")+
  geom_hline(aes(yintercept = hline,
                 group = Island),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_facet() +
  theme(strip.text=element_text(hjust=0))
