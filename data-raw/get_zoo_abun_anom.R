library(tidyverse)
library(readxl)


raw.dir <- here::here("data-raw")
zoo_abund_rdata <- "zoo_abun_anom.rdata"
zoo_strat_abun_xlsx <- "NEFSCZooplankton_v3_7_v2019.xlsx"


get_zoo_abun_anom <- function(save_clean = F){

  load(file.path(raw.dir, zoo_abund_rdata))
  #
  # small <- as.tibble(mtest) %>%
  #   dplyr::filter(!variable == "Cf") %>%
  #   dplyr::group_by(Time, Region) %>%
  #   dplyr::mutate(value = mean(value)) %>%
  #   dplyr::rename(EPU = Region, Value = value, Var = variable) %>%
  #   dplyr::mutate(Var = c("small"))
  #
  # large<- as.tibble(mtest) %>%
  #   dplyr::filter(variable == "Cf") %>%
  #   dplyr::group_by(Time, Region) %>%
  #   dplyr::mutate(value = mean(value)) %>%
  #   dplyr::rename(EPU = Region, Value = value, Var = variable) %>%
  #   dplyr::mutate(Var = c("large"))
  #


  small <- readxl::read_excel(file.path(raw.dir,zoo_strat_abun_xlsx), sheet = "StratifiedAbundance") %>%
    dplyr::select(Year, Units, Region, "Centropages typicus", "Temora longicornis", "Pseudocalanus spp.", "Centropages hamatus" ) %>%
    dplyr::rename(Cty = "Centropages typicus",
           Tlo = "Temora longicornis",
           Pse = "Pseudocalanus spp.",
           Cha =  "Centropages hamatus",
           Time = Year) %>%
    dplyr::group_by(Time) %>%
    dplyr::mutate(Total = sum(Cty +Tlo+Pse+Cha )/4) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Mean = mean(Total),
                  Value = Total - Mean,
                  Var = c("small")) %>%
    dplyr::select(Time, Value, Var, Units, Region)


  large <- readxl::read_excel(file.path(raw.dir,zoo_strat_abun_xlsx), sheet = "StratifiedAbundance") %>%
    dplyr::select(Year, Units, Region, "Calanus finmarchicus" ) %>%
    dplyr::rename(Total = "Calanus finmarchicus",
                  Time = Year) %>%
    dplyr::mutate(Mean = mean(Total),
                  Value = Total - Mean,
                  Var = c("large")) %>%
    dplyr::select(Time, Value, Var, Units, Region)

  zoo_abund <- rbind(small, large) %>%
    dplyr::rename(EPU = Region) %>%
    tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU, Units) %>%
    dplyr::mutate(Units = "Anomaly")

  if (save_clean){
    usethis::use_data(zoo_abund, overwrite = T)
  } else {
    return(zoo_abund)
  }
  # metadata ----
  attr(zoo_abund, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/zooabund.html"
  attr(zoo_abund, "data_files")   <- list(
    zoo_abund_rdata = zoo_abund_rdata)
  attr(zoo_abund, "data_steward") <- c(
    "Harvey Walsh <harvey.walsh@noaa.gov>",
    "Mike Jones <michael.jones@noaa.gov>",
    "Ryan Morse <ryan.morse@noaa.gov>")
}
get_zoo_abun_anom(save_clean = T)


## Stratified Abundance for Small and Large Calanoids, euphasids and cnidarians
get_zoo_strat_abun <- function(save_clean = F){

  zoo_strat_abun <- read_excel(file.path(raw.dir,zoo_strat_abun_xlsx), sheet = "StratifiedAbundance") %>%
    dplyr::select(Year, Units, Region, SmallCalanoida, LargeCalanoida, Euphausiacea, Cnidaria) %>%
    dplyr::rename(Time = Year,
                  EPU = Region) %>%
    gather(Var, Value, SmallCalanoida:Cnidaria)

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
