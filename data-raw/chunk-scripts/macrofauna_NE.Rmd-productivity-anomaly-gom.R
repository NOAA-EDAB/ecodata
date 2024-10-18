
bar_dat_gom <- ecodata::productivity_anomaly %>% 
  dplyr::filter(EPU == "GOM") %>% 
  tidyr::separate(Var, into = c("Var", "Survey"), sep = "_")

gom <- plot_stackbarcpts_single(YEAR = bar_dat_gom$Time,
                         var2bar = bar_dat_gom$Var,
                         x = bar_dat_gom$Value,
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
