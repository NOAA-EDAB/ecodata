
out_pp <- ecodata::chl_pp %>% 
  filter(EPU %in% c("GOM","GB"),
         str_detect(Var, "MONTHLY_PPD_MEDIAN")) %>% 
  separate(.,Time, into = c("Year","Month"), sep = 4) %>% 
    mutate(Month = plyr::mapvalues(Month, from = c("01","02","03","04","05","06",
                                                   "07","08","09","10","11","12"),
                                   to = c(month.abb))) %>% 
  group_by(EPU, Month) %>% 
  mutate(hline = mean(Value))
out_pp$Month <- factor(out_pp$Month, levels = month.abb)

pp_cci_gom <- out_pp %>% 
  filter(EPU == "GOM") %>% 
 ggplot() +
   # geom_gls(aes(x = Year, y = Value, group = Month))+
    geom_point(aes(x = Year, y = Value, group = Month)) +
    geom_line(aes(x = Year, y = Value, group = Month)) +
    scale_x_discrete(name = "", breaks = seq(min(out_pp$Year),max(out_pp$Year),10)) +  
    facet_wrap(Month~., ncol = 12) +
    ggtitle("GOM Monthly median PPD") +
    ylab(expression("PP (gC m"^-2*" d"^-1*")")) +
        geom_hline(aes(yintercept = hline,
                     group = Month),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
    theme_facet() +
    theme(axis.text.x = element_text(angle=45, hjust = 1),
          panel.spacing = unit(1, "lines"),
          plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
 
 pp_cci_gb <-out_pp %>% 
  filter(EPU == "GB") %>% 
 ggplot() +
   # geom_gls(aes(x = Year, y = Value, group = Month))+
    geom_point(aes(x = Year, y = Value, group = Month)) +
    geom_line(aes(x = Year, y = Value, group = Month)) +
    scale_x_discrete(name = "", breaks = seq(min(out_pp$Year),max(out_pp$Year),10)) +  
    facet_wrap(Month~., ncol = 12) +
    ggtitle("GB Monthly median PPD") +
    ylab(expression("PP (gC m"^-2*" d"^-1*")")) +
        geom_hline(aes(yintercept = hline,
                     group = Month),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
    theme_facet() +
    theme(axis.text.x = element_text(angle=45, hjust = 1),
          panel.spacing = unit(1, "lines"),
          plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
 
 pp_cci_gb + pp_cci_gom + plot_layout(ncol = 1)
