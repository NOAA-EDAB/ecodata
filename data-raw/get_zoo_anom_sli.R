# Process zooplankton abundance anomalies and small-large index

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
zoo_sli_anom_rdata <- "zoo_sli_anom.Rdata"

get_zoo_sli_anom <- function(save_clean = F){

  load(file.path(raw.dir, zoo_sli_anom_rdata))

  zoo_sli_anom <- test %>%
  dplyr::rename(EPU = variable, Value = value)

  if (save_clean){
    usethis::use_data(zoo_sli_anom, overwrite = T)
  } else {
    return(zoo_sli_anom)
  }
  # metadata ----
  attr(zoo_sli_anom, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/zooabund.html"
  attr(zoo_sli_anom, "data_files")   <- list(
    zoo_sli_anom_rdata = zoo_sli_anom_rdata)
  attr(zoo_sli_anom, "data_steward") <- c(
    "Harvey Walsh <harvey.walsh@noaa.gov>",
    "Mike Jones <michael.jones@noaa.gov>",
    "Ryan Morse <ryan.morse@noaa.gov>")
}
get_zoo_sli_anom(save_clean = T)




####
zoo_cal_rdata <- "RM_2020_CalanusStage - Ryan Morse - NOAA Affiliate.Rda"
get_calanus_stage <- function(save_clean = F){

  load(file.path(raw.dir, zoo_cal_rdata))

  CalanusStage<- CalanusStage %>%
    dplyr::rename(EPU = epu)
  if (save_clean){
    usethis::use_data(CalanusStage, overwrite = T)
  } else {
    return(CalanusStage)
  }
  # metadata ----
  attr(CalanusStage, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/zooabund.html"
  attr(CalanusStage, "data_files")   <- list(
    zoo_cal_rdata = zoo_cal_rdata)
  attr(CalanusStage, "data_steward") <- c(
    "Ryan Morse <ryan.morse@noaa.gov>")
}
get_calanus_stage(save_clean = T)
