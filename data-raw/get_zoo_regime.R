## Get zooplankton regime shifts

library(tidyverse)



raw.dir <- here::here("data-raw")
gom_zoo <- "GOM_mean_annual_anomalies (1).rdata"
gb_zoo <- "GBK_mean_annual_anomalies (1).rdata"
mab_zoo <- "MAB_mean_annual_anomalies (1).rdata"


get_zoo_regime <- function(save_clean = F){

  load(file.path(raw.dir, gom_zoo))
  gom<- gom.yr.mn %>% dplyr::mutate(EPU = c('GOM'))
  load(file.path(raw.dir, gb_zoo))
  gb<- gbk.yr.mn %>% dplyr::mutate(EPU = c('GB'))
  load(file.path(raw.dir, mab_zoo))
  mab<- mab.yr.mn %>% dplyr::mutate(EPU = c('MAB'))


  zoo_regime <- rbind(gom, gb, mab ) %>%
    dplyr::filter(Var!='sfc_temp', Var!='sfc_salt', Var!='btm_temp',
           Var!='btm_salt', Var!='volume_100m3') %>%
    dplyr::rename("Time" = "year") %>%
    tibble::as_tibble()

  if (save_clean){
    usethis::use_data(zoo_regime, overwrite = T)
  } else {
    return(zoo_regime)
  }
}
get_zoo_regime(save_clean = T)

