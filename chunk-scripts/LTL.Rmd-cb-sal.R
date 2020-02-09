
ches_sal<-ecodata::ch_bay_sal %>% 
  filter(!Var == "UTCTime") %>% 
  drop_na() %>%
  mutate(Time =  as.numeric(str_sub(Time, 2, -1)),
         Time1 = as.Date(Time, origin = "2018-12-31")) %>% 
  pivot_wider(names_from = Var, values_from = Value)

ches_sal %>% 
  ggplot() +
  geom_ribbon(aes(x = Time1, ymin = AvgMinLim, ymax = AvgMaxLim))+
  geom_ribbon(aes(x = Time1, ymin = MinDataLim, ymax = MaxDataLim), alpha = 0.3)+
  geom_line(aes(x = Time1, y = Daily18), color = "blue") +
  geom_line(aes(x = Time1, y = Daily19), color = "red") +
  ylab(expression("PSU")) +
  ggtitle("Chesapeake Bay Salinity") +
  theme_ts()
