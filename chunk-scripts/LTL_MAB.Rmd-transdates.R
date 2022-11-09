
ecodata::trans_dates %>% 
  dplyr::filter(EPU == "MAB", 
                Var %in% c("falltrans", "sprtrans", "maxday"), 
                !Value == "NA", 
                !Var == "NA", 
                !Time == "NA") %>%
  dplyr::mutate(Var = recode(Var, "falltrans"="Fall", 
                             "sprtrans" = "Spring", 
                             "maxday" = "Max")) %>% 
  ggplot(aes(x= Time, y = Value, color = Var))+
    geom_point()+
    geom_line()+
    ecodata::geom_gls() +
    ggplot2::theme(strip.text=element_text(hjust=0),
                 plot.title = element_text(size = 12))+
    ecodata::theme_title()+
    ylab("Day of Year")+
    xlab(element_blank())+
    ecodata::theme_ts()
