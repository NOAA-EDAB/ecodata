
interp_chl_pp <- function(epu, year = 2021, Variable){
  out <- ecodata::chl_pp %>%
    dplyr::filter(stringr::str_detect(Var,Variable),
           EPU == epu) %>%
    tidyr::separate(.,Time, into = c("Cat", "Time2"), sep = "_") %>%
    tidyr::separate(.,Time2, into = c("Year", "Week"), sep = 4)%>%
    dplyr::filter(Year == year) %>%
    dplyr::group_by(EPU) %>%
    dplyr::mutate(Time = 1:length(Year))

  ltm_out <- ecodata::chl_pp %>%
    dplyr::filter(stringr::str_detect(Var,Variable),
           EPU == epu) %>%
    tidyr::separate(.,Time, into = c("Cat", "Time2"), sep = "_") %>%
    tidyr::separate(.,Time2, into = c("Year", "Week"), sep = 4) %>%
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

GB_chl_weekly <- interp_chl_pp(epu = "GB", Variable = "WEEKLY_CHLOR_A_MEDIAN")
GOM_chl_weekly <- interp_chl_pp(epu = "GOM", Variable = "WEEKLY_CHLOR_A_MEDIAN")
ne_chl_weekly<-rbind(GB_chl_weekly, GOM_chl_weekly)%>% 
  dplyr::filter(!Value == "NA")
# 
 ne_early<-ne_chl_weekly %>% filter(Time <=18)
 ne_late<-ne_chl_weekly %>% filter(Time >=18)

ne_chl <- ggplot2::ggplot(data = ne_early) +
  ggplot2::geom_line(aes(x = Time, y = LTM)) +
  ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(sd.low,0), ymax = sd.high),
              alpha = 0.1,
              fill = "grey1") +
  ggplot2::geom_line(aes(x = Time, y = Value),
            size = 1,color = "#33a02c") +
   ggplot2::geom_line(data = ne_late, aes(x = Time, y = LTM)) +
   ggplot2::geom_ribbon(data = ne_late,aes(x = Time, ymin = pmax(sd.low,0), ymax = sd.high),
               alpha = 0.1,
               fill = "grey1") +
   ggplot2::geom_line(data = ne_late,aes(x = Time, y = Value),
             size = 1,color = "#33a02c", linetype = "dashed") +
  ggplot2::ggtitle(expression("Chlorophyll"~italic(a)~" 2020 New Data")) +
  ggplot2::ylim(c(0, 3))+
  ggplot2::facet_wrap(EPU~., ncol = 2)+
  ggplot2::guides(color = F) +
  ggplot2::xlab("")+
  ggplot2::ylab("mg m^-3") +
  ggplot2::scale_x_continuous(breaks = seq(1,52,10),
                   labels = c("Jan.","Mar.","May","July","Oct.","Dec."),
                   expand = c(0.01,0.01)) +
  ggplot2::scale_color_manual(values = c("#ef8a62","#2c7fb8","#a1d99b"))+
  ecodata::theme_ts()+
  ecodata::theme_title()+
  ecodata::theme_facet()

ne_chl
