
raw.dir<-here::here("data-raw")
bird<- ecodata::seabird_MAB %>% 
  mutate(hline = mean(Value))



# seabird <- ecodata::commontern_MAB %>% 
#   mutate(Value = as.numeric(Value),
#          Time = as.numeric(Time)) %>% 
#   group_by(Time) %>% 
#   dplyr::summarise(Mean = mean(Value, na.rm = T),
#                    SE = sd(Value, na.rm = T)/sqrt(n()),
#                    SD = sd(Value, na.rm = T),
#                    n = n()) %>% 
#   mutate(Mean = ifelse(is.na(SE),NA,Mean),
#          se.low = Mean - SE,
#          se.high = Mean + SE,
#          hline = mean(Mean, na.rm = T))

bird %>% 
  ggplot() +
#Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = Value, color = Group), size = lwd-0.75) +
  geom_point(aes(x = Time, y = Value, color = Group), size = pcex-0.75) +
  #geom_gls(aes(x = Time, y = Mean)) +
  #geom_errorbar(aes(x = Time,
   #                 ymin = se.low,
  #                ymax = se.high), 
  #              width = 0.25) +
  scale_x_continuous(expand = c(0.01, 0.01),limits = c(1991,2018)) +
  ggtitle("Seabird Abundance") +
  ylab(expression("Number of Breeding Pairs")) +
  xlab("")+
  #geom_hline(aes(yintercept = hline),
  #         color = "black",
  #        size = hline.size,
  #         alpha = hline.alpha,
  #         linetype = hline.lty) +
  theme(legend.position="bottom", legend.direction = "horizontal", 
        legend.title = element_blank(), legend.margin=margin(t = -20)) +
  theme_ts()
