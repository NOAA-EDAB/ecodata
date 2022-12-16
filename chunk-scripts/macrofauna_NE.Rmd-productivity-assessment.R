
test<- ecodata::productivity_anomaly %>% 
  tidyr::separate(Var, into = c("Stock", "Var"), sep = "-")  %>% 
  dplyr::filter(EPU == "NE", 
                Var == "rs_anom") %>% 
  group_by(Time) %>% 
  dplyr::mutate(total = sum(Value, na.rm = TRUE))
  

p <-   
    ggplot2::ggplot(test,
           aes(x = Time)) +
    ggplot2::geom_bar(data = test %>% filter(Value > 0),
             aes(y = Value, fill = Stock),
             stat = "identity") +
    ggplot2::geom_bar(data = test %>% filter(Value < 0),
             aes(y = Value, fill = Stock),
             stat = "identity") +
    ggplot2::geom_line(data = test, aes(x = Time, y = total),
                             size = 1) +
    ggplot2::geom_hline(size = 0.3, aes(yintercept = 0)) +
    ggplot2::xlab("") +
    ggplot2::ylab("Recruitment Anomaly") +
    ggplot2::ggtitle("NE Recruitment Anomaly from Stock Assessments") +
    #ggplot2::guides(fill = guide_legend(ncol = leg_ncol)) +
    ecodata::theme_ts()+
    ggplot2::theme(axis.title   = element_text(size = 10),
          axis.text    = element_text(size = 10),
          plot.title   = element_text(size = 12),
          #legend.text  = element_text(size = leg_font_size),
          legend.title = element_blank()) 
p
