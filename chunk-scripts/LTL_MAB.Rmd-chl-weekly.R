
interp_chl_pp <- function(epu, year = 2020, Variable){
  out <- ecodata::chl_pp %>% 
    dplyr::filter(stringr::str_detect(Var,Variable),
           EPU == epu) %>% 
    tidyr::separate(.,Time, into = c("Cat", "Time2"), sep = "_") %>% 
    tidyr::separate(.,Time2, into = c("Year", "Week"), sep = 4) %>% 
    dplyr::filter(Year == year) %>% 
    dplyr::group_by(EPU) %>% 
    dplyr::mutate(Time = 1:length(Year))
  
  ltm_out <- ecodata::chl_pp %>% 
    dplyr::filter(stringr::str_detect(Var,Variable),
           EPU == epu) %>% 
    tidyr::separate(.,Time, into = c("Cat", "Time2"), sep = "_") %>% 
    tidyr::separate(.,Time2, into = c("Year", "Week"), sep = 4)  %>% 
    dplyr::group_by(Week) %>% 
    dplyr::summarise(LTM = mean(Value, na.rm = T),
                     SD = sd(Value, na.rm = T)) %>% 
    dplyr::mutate(Time = 1:length(Week),
           sd.low = LTM - SD,
           sd.high = LTM + SD) %>% 
    dplyr::left_join(.,out, by = c("Time")) %>% 
    dplyr::mutate(status = ifelse(Value < sd.high & Value > sd.low, "near_mean",
                           ifelse(Value > sd.high, "high",
                                  ifelse(Value < sd.low,"low",NA))),
           group = "PLOT")
  
  return(ltm_out)
}

MAB_chl <- interp_chl_pp(epu = "MAB", Variable = "WEEKLY_CHLOR_A_MEDIAN")
MAB_early<- MAB_chl %>% filter(Week.x <=26)
MAB_late <- MAB_chl %>% filter(Week.x >=26)

MAB_chl_weekly <- ggplot2::ggplot(data = MAB_early) +
  ggplot2::geom_line(aes(x = Time, y = LTM)) +
  ggplot2::geom_ribbon(aes(x = Time, ymin = sd.low, ymax = sd.high), 
              alpha = 0.1,
              fill = "grey1") +
  ggplot2::geom_line(data = MAB_late,aes(x = Time, y = LTM)) +
  ggplot2::geom_ribbon(data = MAB_late,aes(x = Time, ymin = sd.low, ymax = sd.high), 
              alpha = 0.1,
              fill = "grey1")+
  ggplot2::geom_line(aes(x = Time, y = Value),
            size = 1,color = "#33a02c") +
    ggplot2::geom_line(data = MAB_late, aes(x = Time, y = Value),
            size = 1,color = "#33a02c", linetype = "dashed")+
  ggplot2::ggtitle(expression("Chlorophyll"~italic(a)~"")) +
  ggplot2::guides(color = F) +
  ggplot2::xlab("")+
  ggplot2::ylab(expression("CHL (mg m"^-3*")")) +
  ggplot2::scale_x_continuous(breaks = seq(1,52,10),
                   labels = c("Jan.","Mar.","May","July","Oct.","Dec."),
                   expand = c(0.01,0.01)) +
  ggplot2::scale_color_manual(values = c("#ef8a62","#2c7fb8","#a1d99b"))+
  ecodata::theme_ts()+
  ecodata::theme_title()

MAB_pp <- interp_chl_pp(epu = "MAB", Variable =  "WEEKLY_PPD_MEDIAN")
MAB_early<- MAB_pp %>% filter(Week.x <=26)
MAB_late <- MAB_pp %>% filter(Week.x >=26)

MAB_pp_weekly <- ggplot2::ggplot(data = MAB_early) +
  ggplot2::geom_line(aes(x = Time, y = LTM)) +
  ggplot2::geom_ribbon(aes(x = Time, ymin = sd.low, ymax = sd.high),
              alpha = 0.1,
              fill = "grey1") +
  ggplot2::geom_line(data = MAB_late,aes(x = Time, y = LTM)) +
  ggplot2::geom_ribbon(data = MAB_late,aes(x = Time, ymin = sd.low, ymax = sd.high), 
              alpha = 0.1,
              fill = "grey1")+
  ggplot2::geom_line(aes(x = Time, y = Value),
            size = 1,color = "#33a02c") +
  ggplot2::geom_line(data = MAB_late, aes(x = Time, y = Value),
            size = 1,color = "#33a02c", linetype = "dashed")+
  ggplot2::ggtitle(expression("Primary production")) +
  ggplot2::guides(color = F) +
  ggplot2::xlab("")+
  ggplot2::ylab(expression("PP (gC m"^-2*" d"^-1*")")) +
  ggplot2::scale_x_continuous(breaks = seq(1,52,10),
                   labels = c("Jan.","Mar.","May","July","Oct.","Dec."),
                   expand = c(0.01,0.01)) +
  ggplot2::scale_color_manual(values = c("#ef8a62","#2c7fb8","#a1d99b"))+
  ecodata::theme_ts()+
  ecodata::theme_title()

MAB_pp_weekly + MAB_chl_weekly + plot_layout(ncol = 1)
