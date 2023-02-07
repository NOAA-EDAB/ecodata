library(tidyverse)
library(readxl)


raw.dir <- here::here("data-raw")
zoo_abundance_anom_xlsx <- "NEFSCZooplankton_v3_8_anomalies.xlsx"

get_zoo_abundance <- function(save_clean = F){

  zoo_abundance_anom<- readxl::read_excel(file.path(raw.dir, zoo_abundance_anom_xlsx)) %>%
    dplyr::mutate(Var = paste0(Taxa),
                  EPU = Region,
                  Time = Year) %>%
    dplyr::select(Time, Var, Value, EPU, Units)


  if (save_clean){
    usethis::use_data(zoo_abundance_anom, overwrite = T)
  } else {
    return(zoo_abundance_anom)
  }
}
get_zoo_abundance(save_clean = T)


# Stratified Abundance euphasids and cnidarians

zoo_strat_abun_xlsx <- "NEFSCZooplankton_v3_8_abundance_krill_cnidaria.xlsx"
get_zoo_strat_abun <- function(save_clean = F){

  zoo_strat_abun <- readxl::read_excel(file.path(raw.dir,zoo_strat_abun_xlsx)) %>%
    dplyr::select(Year, Units, Region, Euphausiacea, Cnidaria) %>%
    dplyr::rename(Time = Year,
                  EPU = Region) %>%
    tidyr::pivot_longer(c("Euphausiacea","Cnidaria"),names_to = "Var", values_to = "Value")

  if (save_clean){
    usethis::use_data(zoo_strat_abun, overwrite = T)
  } else {
    return(zoo_strat_abun)
  }
  # metadata ----
  attr(zoo_strat_abun, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/chl-pp.html"
  attr(zoo_strat_abun, "data_files")   <- list(
    zoo_strat_abun_xlsx = zoo_strat_abun_xlsx)
  attr(zoo_strat_abun, "data_steward") <- c(
    "Harvey Walsh <harvey.walsh@noaa.gov>",
    "Mike Jones <michael.jones@noaa.gov>",
    "Ryan Morse <ryan.morse@noaa.gov>")
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
