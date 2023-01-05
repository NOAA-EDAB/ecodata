
total<- ecodata::productivity_anomaly %>% 
  tidyr::separate(Var, into = c("Stock", "Var"), sep = "-")  %>% 
  dplyr::filter(EPU == "MA", 
                Var == "rs_anom") %>% 
  dplyr::group_by(Time) %>% 
  dplyr::summarise(Total = sum(Value, na.rm = T),
                      Count = n()) %>% # SG add a count of species
  dplyr::mutate(Totalold = ifelse(Total == 0, NA, Total),
            Total = ifelse(Count < max(Count), NA, Total))
  
prod<- ecodata::productivity_anomaly %>% 
  tidyr::separate(Var, into = c("Stock", "Var"), sep = "-")  %>% 
  dplyr::filter(EPU == "MA", 
                Var == "rs_anom")

p <-   
    ggplot2::ggplot(prod,
           aes(x = Time)) +
    ggplot2::geom_bar(data = prod %>% filter(Value > 0),
             aes(y = Value, fill = Stock),
             stat = "identity") +
    ggplot2::geom_bar(data = prod %>% filter(Value < 0),
             aes(y = Value, fill = Stock),
             stat = "identity") +
    ggplot2::geom_line(data = total, aes(x = Time, y = Total),
                             size = 1) +
    ggplot2::geom_hline(size = 0.3, aes(yintercept = 0)) +
    ggplot2::xlab("") +
    ggplot2::ylab("Recruitment Anomaly") +
    ggplot2::ggtitle("MAB Recruitment Anomaly from Stock Assessments") +
    #ggplot2::guides(fill = guide_legend(ncol = leg_ncol)) +
    ecodata::theme_ts()+
    ggplot2::theme(axis.title   = element_text(size = 10),
          axis.text    = element_text(size = 10),
          plot.title   = element_text(size = 12),
          #legend.text  = element_text(size = leg_font_size),
          legend.title = element_blank()) 
p
