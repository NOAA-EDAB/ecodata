
interp_chl_pp <- function(epu, year = 2019, Variable){
  out <- ecodata::chl_pp %>% 
    filter(str_detect(Var,Variable),
           EPU == epu) %>% 
    separate(.,Time, c("Year","Week"),sep = 4) %>% 
    filter(Year == year) %>% 
    group_by(EPU) %>% 
    mutate(Time = 1:length(Year))
  
  ltm_out <- ecodata::chl_pp %>% 
    filter(str_detect(Var,Variable),
           EPU == epu) %>% 
    separate(.,Time, c("Year","Week"),sep = 4) %>% 
    group_by(Week) %>% 
    dplyr::summarise(LTM = mean(Value, na.rm = T),
                     SD = sd(Value, na.rm = T)) %>% 
    mutate(Time = 1:length(Week),
           sd.low = LTM - SD,
           sd.high = LTM + SD) %>% 
    left_join(.,out, by = c("Time")) %>% 
    mutate(status = ifelse(Value < sd.high & Value > sd.low, "near_mean",
                           ifelse(Value > sd.high, "high",
                                  ifelse(Value < sd.low,"low",NA))),
           group = "PLOT")
  
  return(ltm_out)
}

MAB_chl <- interp_chl_pp(epu = "MAB", Variable = "WEEKLY_CHLOR_A_MEDIAN MODIS-Aqua PAN")

MAB_chl_weekly <- ggplot(data = MAB_chl) +
  geom_line(aes(x = Time, y = LTM)) +
  geom_ribbon(aes(x = Time, ymin = sd.low, ymax = sd.high), 
              alpha = 0.1,
              fill = "grey1") +
  geom_line(aes(x = Time, y = Value),
            size = 1,color = "#33a02c") +
  ggtitle(expression("Chlorophyll"~italic(a)~"")) +
  guides(color = F) +
  xlab("")+
  ylab(expression("CHL (mg m"^-3*")")) +
  scale_x_continuous(breaks = seq(1,52,10),
                   labels = c("Jan.","Mar.","May","July","Oct.","Dec."),
                   expand = c(0.01,0.01)) +
  scale_color_manual(values = c("#ef8a62","#2c7fb8","#a1d99b"))+
  theme_ts()

MAB_pp <- interp_chl_pp(epu = "MAB", Variable =  "WEEKLY_PPD_MEDIAN")

MAB_pp_weekly <- ggplot(data = MAB_pp) +
  geom_line(aes(x = Time, y = LTM)) +
  geom_ribbon(aes(x = Time, ymin = sd.low, ymax = sd.high),
              alpha = 0.1,
              fill = "grey1") +
  geom_line(aes(x = Time, y = Value),
            size = 1,color = "#33a02c") +
  ggtitle(expression("Primary production")) +
  guides(color = F) +
  xlab("")+
  ylab(expression("PP (gC m"^-2*" d"^-1*")")) +
  scale_x_continuous(breaks = seq(1,52,10),
                   labels = c("Jan.","Mar.","May","July","Oct.","Dec."),
                   expand = c(0.01,0.01)) +
  scale_color_manual(values = c("#ef8a62","#2c7fb8","#a1d99b"))+
  theme_ts()

MAB_chl_weekly + MAB_pp_weekly + plot_layout(ncol = 1)
