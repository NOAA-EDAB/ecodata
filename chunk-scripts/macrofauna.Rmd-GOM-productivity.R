
## GOM
bar_dat <- ecodata::productivity_anomaly %>% 
  filter(EPU == "GOM")

gom <- plot_stackbarcpts_single(YEAR = bar_dat$Time,
                         var2bar = bar_dat$Var,
                         x = bar_dat$Value,
                         titl = "Gulf of Maine",
                         xlab = "",
                         ylab = "Small fish per large fish biomass (anomaly)",
                         height = 5.5,
                         width = 9,
                         filt = FALSE,
                         label = "",
                         y.text = 10,
                         aggregate = TRUE)
gom
