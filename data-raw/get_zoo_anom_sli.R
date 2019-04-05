# Process zooplankton abundance anomalies and small-large index 

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")

get_zoo_anom_sli <- function(save_clean = F){
  
  load(file.path(raw.dir, "1977_2017_SLI_Calfin_Pseudo_Ctyp.Rdata"))
  
  zoo_anom_sli <- Zooplankton_Primary_Prod %>%
  gather(.,Var, Value, -year) %>% 
  dplyr::rename(Time = year) %>% 
  separate(., Var, c("Var","EPU")) %>% 
  mutate(EPU = plyr::mapvalues(EPU, from = c("gom","gbk","scs","mab"), 
                               to = c("GOM","GB","SS","MAB")),
         Var = plyr::mapvalues(Var, from = c("PseAnom",
                                             "CtyAnom",
                                             "CalAnom",
                                             "SLI"), 
                               to = c("pseudocalanus anomaly",
                                      "centropages anomaly",
                                      "calanus anomaly",
                                      "small-large index")),
         Units = "anomaly")
  
  if (save_clean){
    usethis::use_data(zoo_anom_sli, overwrite = T)
  } else {
    return(zoo_anom_sli)
  }
}
get_zoo_anom_sli(save_clean = T)
