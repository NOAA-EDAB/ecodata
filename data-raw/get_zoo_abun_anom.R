library(tidyverse)


raw.dir <- here::here("data-raw")

get_zoo_abun_anom <- function(save_clean = F){

  load(file.path(raw.dir, "zoo_abun_anom.rdata"))

  small <- as.tibble(mtest) %>%
    dplyr::filter(!variable == "Cf") %>%
    dplyr::group_by(Time, Region) %>%
    dplyr::mutate(value = mean(value)) %>%
    dplyr::rename(EPU = Region, Value = value, Var = variable) %>%
    dplyr::mutate(Var = c("small"))

  large<- as.tibble(mtest) %>%
    dplyr::filter(variable == "Cf") %>%
    dplyr::group_by(Time, Region) %>%
    dplyr::mutate(value = mean(value)) %>%
    dplyr::rename(EPU = Region, Value = value, Var = variable) %>%
    dplyr::mutate(Var = c("large"))

  zoo_abund <- rbind(small, large)

  if (save_clean){
    usethis::use_data(zoo_abund, overwrite = T)
  } else {
    return(zoo_abund)
  }
}
get_zoo_abun_anom(save_clean = T)


## Stratified Abundance for Small and Large Calanoids, euphasids and cnidarians
get_zoo_strat_abun <- function(save_clean = F){

  zoo_strat_abun <- read_excel(file.path(raw.dir,"NEFSCZooplankton_v3_6b_v2018.xlsx"), sheet = "StratifiedAbundance") %>%
    dplyr::select(Year, Units, Region, SmallCalanoida, LargeCalanoida, Euphausiacea, Cnidaria) %>%
    dplyr::rename(Time = Year,
                  EPU = Region) %>%
    gather(Var, Value, SmallCalanoida:Cnidaria)

  if (save_clean){
    usethis::use_data(zoo_strat_abun, overwrite = T)
  } else {
    return(zoo_strat_abun)
  }
}
get_zoo_strat_abun(save_clean = T)

### Tried to plot up mean of all the anomalies but this is incorrect because
## We don't have orig data should be anom of mean not mean of anom.

# get_sm_vs_calanus_abun<- function(save_clean = F){
#
#   load(file.path(raw.dir, "zoo_abun_anom.rdata"))
#
#   sm_vs_calanus_abun <- as.tibble(mtest) %>%
#     dplyr::rename(EPU = Region, Value = value, Var = variable) %>%
#     mutate(Var = plyr::mapvalues(Var, from = c("Pse", "Cty", "Cha", "Tlo", "Cf"),
#                                  to = c("Small", "Small", "Small",
#                                         "Small", "Calanus"))) %>%
#     group_by(EPU, Time, Var) %>%
#     summarise(Value = mean(Value))
#   if (save_clean){
#     usethis::use_data(sm_vs_calanus_abun, overwrite = T)
#   } else {
#     return(sm_vs_calanus_abun)
#   }
# }
