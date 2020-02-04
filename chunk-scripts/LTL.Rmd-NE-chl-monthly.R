
## out_chl <- ecodata::chl_pp %>%
##   filter(EPU %in% c("GOM","GB"),
##          str_detect(Var, "MONTHLY_CHLOR_A_MEDIAN")) %>%
##   separate(.,Time, into = c("Year","Month"), sep = 4) %>%
##     mutate(Month = plyr::mapvalues(Month, from = c("01","02","03","04","05","06",
##                                                    "07","08","09","10","11","12"),
##                                    to = c(month.abb)))
## out_chl$Month <- factor(out_chl$Month, levels = month.abb)
## 
## 
## chl_cci_gom <-
##   out_chl %>%
##   filter(EPU == "GOM") %>%
##   ggplot() +
##     #geom_gls(aes(x = Year, y = Value, group = Month))+
##     geom_point(aes(x = Year, y = Value, group = Month)) +
##     geom_line(aes(x = Year, y = Value, group = Month)) +
##     scale_x_discrete(name = "", breaks = seq(min(out_chl$Year),max(out_chl$Year),10)) +
##     facet_wrap(Month~., ncol = 12) +
##     ggtitle("GOM Monthly median CHL (OC-CCI)") +
##     ylab(expression("CHL (mg m"^-3*")")) +
##     theme_facet() +
##     theme(axis.text.x = element_text(angle=45, hjust = 1),
##           panel.spacing = unit(0.5, "lines"),
##           plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
## 
## chl_cci_gb <-
##   out_chl %>%
##   filter(EPU == "GB") %>%
##   ggplot() +
##     #geom_gls(aes(x = Year, y = Value, group = Month))+
##     geom_point(aes(x = Year, y = Value, group = Month)) +
##     geom_line(aes(x = Year, y = Value, group = Month)) +
##     scale_x_discrete(name = "", breaks = seq(min(out_chl$Year),max(out_chl$Year),10)) +
##     facet_wrap(Month~., ncol = 12) +
##     ggtitle("GB Monthly median CHL (OC-CCI)") +
##     ylab(expression("CHL (mg m"^-3*")")) +
##     theme_facet() +
##     theme(axis.text.x = element_text(angle=45, hjust = 1),
##           panel.spacing = unit(0.5, "lines"),
##           plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
## chl_cci_gb + chl_cci_gom + plot_layout(ncol = 1)
## 
