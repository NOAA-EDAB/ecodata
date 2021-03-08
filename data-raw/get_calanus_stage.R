### get calanus stage data

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
####
zoo_cal_rdata <- "RM_20210205_CalanusStage.Rda"
get_calanus_stage <- function(save_clean = F){

  load(file.path(raw.dir, zoo_cal_rdata))

  calanus_stage<- CalanusStage2 %>%
    dplyr::rename(EPU = epu)

  if (save_clean){
    usethis::use_data(calanus_stage, overwrite = T)
  } else {
    return(calanus_stage)
  }
  # metadata ----
  attr(calanus_stage, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/zooabund.html"
  attr(calanus_stage, "data_files")   <- list(
    zoo_cal_rdata = zoo_cal_rdata)
  attr(calanus_stage, "data_steward") <- c(
    "Ryan Morse <ryan.morse@noaa.gov>")
}
get_calanus_stage(save_clean = T)
