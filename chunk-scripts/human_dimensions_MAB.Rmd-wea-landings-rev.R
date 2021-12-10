
dt<- ecodata::wea_landings_rev[1:10, 1:3] %>% 
  dplyr::rename("Maximum Percent Total Annual Regional Species Landings"="perc_landings",
    "Maximum Percent Total Annual Regional Species Revenue"="perc_revenue" ) 
kable(dt, caption = "Top ten species Landings and Revenue from Wind Energy Areas.") %>% 
  kable_classic(full_width = F, html_font = "Cambria")
