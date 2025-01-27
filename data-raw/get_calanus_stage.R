### get calanus stage data

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
####
zoo_cal_rdata <- "20241223_CalanusStageAnom - Ryan Morse - NOAA Affiliate.RData"
get_calanus_stage <- function(save_clean = F){

  load(file.path(raw.dir, zoo_cal_rdata))

  calanus_stage<- CalanusStage %>%
    dplyr::rename(EPU = epu,
                  Time = YEAR) %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  # metadata ----
  attr(calanus_stage, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/zooabund.html"
  attr(calanus_stage, "data_files")   <- list(
    zoo_cal_rdata = zoo_cal_rdata)
  attr(calanus_stage, "data_steward") <- c(
    "Ryan Morse <ryan.morse@noaa.gov>")
  attr(calanus_stage, "plot_script") <- list(
    `ltl_MAB` = "LTL_MAB.Rmd-calanus-stage.R",
    `ltl_NE_gb` = "LTL_NE.Rmd-gb-calanus-stage.R",
    `ltl_NE_gom` = "LTL_NE.Rmd-gom-calanus-stage.R",
    `ltl_NE_gom-old` = "LTL_NE.Rmd-gom-calanus-stage-old.R")

  if (save_clean){
    usethis::use_data(calanus_stage, overwrite = T)
  } else {
    return(calanus_stage)
  }
}
get_calanus_stage(save_clean = T)
