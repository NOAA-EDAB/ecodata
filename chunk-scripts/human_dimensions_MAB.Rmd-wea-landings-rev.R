
dt<- ecodata::wea_landings_rev[1:10, 1:5] %>% 
  dplyr::select("GARFO and ASMFC Managed Species", "perc_landings_max"  ,"perc_landings_min",
                "perc_revenue_max","perc_revenue_min" ) %>% 
  dplyr::rename("Maximum Percent Total Annual Regional Species Landings"="perc_landings_max",
    "Maximum Percent Total Annual Regional Species Revenue"="perc_revenue_max",
    "Minimum Percent Total Annual Regional Species Landings"="perc_landings_min",
    "Minimum Percent Total Annual Regional Species Revenue"="perc_revenue_min",) 
kable(dt, caption = "Top ten species Landings and Revenue from Wind Energy Areas.") %>% 
  kable_classic(full_width = F, html_font = "Cambria")
