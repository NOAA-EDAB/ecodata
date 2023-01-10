
#dt<- ecodata::wea_landings_rev[1:10, 1:5] %>% 
dt<- ecodata::wea_landings_rev %>% 
  dplyr::select("NEFMC, MAFMC, and ASMFC Managed Species", 
                "perc_landings_max","perc_revenue_max" ) %>% 
 dplyr::slice_head(n =10) %>% 
  dplyr::mutate(perc_landings_max = paste0(perc_landings_max, " %"),
                perc_revenue_max = paste0(perc_revenue_max, " %")) %>% 
  dplyr::rename("Maximum Percent Total Annual Regional Species Landings"="perc_landings_max",
    "Maximum Percent Total Annual Regional Species Revenue"="perc_revenue_max") 
kable(dt, caption = "Top ten species Landings and Revenue from Wind Energy Areas." ) %>% 
  kable_classic(full_width = F, html_font = "Cambria")
