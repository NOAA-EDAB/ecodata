
out_chl <- ecodata::chl_pp %>% 
  dplyr::filter(EPU == epu_abbr,
         stringr::str_detect(Var, "MONTHLY_CHLOR_A_MEDIAN")) %>% 
  tidyr::separate(.,Time, into = c("Cat", "Time2"), sep = "_") %>% 
  tidyr::separate(.,Time2, into = c("Year", "Month"), sep = 4)%>% 
    dplyr::mutate(Month = plyr::mapvalues(Month, from = c("01","02","03","04","05","06",
                                                   "07","08","09","10","11","12"),
                                   to = c(month.abb)))%>% 
  dplyr::filter(!Value == "NA") %>% 
  dplyr::group_by(Month) %>% 
  dplyr::mutate(hline = mean(Value))

out_chl$Month <- factor(out_chl$Month, levels = month.abb)


chl_cci <- ggplot2::ggplot(out_chl) +
   # geom_gls(aes(x = Year, y = Value, group = Month))+
    ggplot2::geom_point(aes(x = Year, y = Value, group = Month)) +
    ggplot2::geom_line(aes(x = Year, y = Value, group = Month)) +
    ggplot2::scale_x_discrete(name = "", breaks = seq(min(out_chl$Year),max(out_chl$Year),10)) +  
            geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
    ggplot2::facet_wrap(Month~., ncol = 12) +
    ggplot2::ggtitle("Monthly median CHL") +
    ggplot2::ylab(expression("CHL (mg m"^-3*")")) +
    ecodata::theme_facet() +
    ggplot2::theme(axis.text.x = element_text(angle=45, hjust = 1),
          panel.spacing = unit(.5, "lines"),
          plot.margin = unit(c(0.1, 0, 0, 0), "cm"))+
  ecodata::theme_title()

chl_cci
