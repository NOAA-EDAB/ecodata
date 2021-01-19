# Process ocean temperature anomaly data

# These data include in situ regional time series of
# both surface and bottom water temperature anomalies
# on the Northeast Continental Shelf. Raw data is split
# into four files by EPU (SS, GOM, GB, and MAB).


library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

#Get raw
raw.dir <- here::here("data-raw") #input raw

bottom_temp_GOM_csv<-"bot_temp_GOM.csv"
bottom_temp_GB_csv<-"bot_temp_GB.csv"
bottom_temp_MAB_csv<-"bot_temp_MAB.csv"
bottom_temp_SS_csv<-"bot_temp_SS.csv"

get_bottom_temp <- function(save_clean = F){

  ss <- read.csv(file.path(raw.dir,bottom_temp_SS_csv)) %>% mutate(EPU = "SS")
  gom <- read.csv(file.path(raw.dir,bottom_temp_GOM_csv)) %>% mutate(EPU = "GOM")
  gb <- read.csv(file.path(raw.dir,bottom_temp_GB_csv)) %>% mutate(EPU = "GB")
  mab <- read.csv(file.path(raw.dir,bottom_temp_MAB_csv)) %>% mutate(EPU = "MAB")

  bottom_temp <- rbind(ss, gom, gb, mab) %>% #bind all
    dplyr::mutate(Units = "degreesC", Time = as.Date(format(date_decimal(Time), "%Y-%b-%d"), "%Y-%b-%d"),
           Var, Var = plyr::mapvalues(Var, from = c("Tsfc_anom",#Rename variables
                                                    "Tsfc_ref",
                                                    "Tbot_anom",
                                                    "Tbot_ref"),
                                      to = c("sst anomaly in situ",
                                             "reference sst in situ (1981-2010)",
                                             "bottom temp anomaly in situ",
                                             "reference bt in situ (1981-2010)"))) %>%
    dplyr::group_by(Time = year(Time), EPU, Var, Units) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    as.data.frame()

  if (save_clean){
    usethis::use_data(bottom_temp, overwrite = T)
  } else {
    return(bottom_temp)
  }

  # metadata ----
  attr(bottom_temp, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/bottom-temperatures.html"
  attr(bottom_temp, "data_files")   <- list(
    bottom_temp_SS_csv = bottom_temp_SS_csv,
    bottom_temp_GB_csv = bottom_temp_GB_csv,
    bottom_temp_GOM_csv = bottom_temp_GOM_csv,
    bottom_temp_MAB_csv = bottom_temp_MAB_csv)
  attr(bottom_temp, "data_steward") <- c(
    "Paula Fratantoni <paula.fratantoni@noaa.gov>")
}
get_bottom_temp(save_clean = T)






#### GLORYS Data

bottom_temp_glorys_csv<-"GLORYS12v1_bottom_temp_anomaly_EPU - Joseph Caracappa - NOAA Affiliate.csv"


get_bottom_temp_glorys <- function(save_clean = F){

  bottom_temp_glorys <- read.csv(file.path(raw.dir,bottom_temp_glorys_csv))

  if (save_clean){
    usethis::use_data(bottom_temp_glorys, overwrite = T)
  } else {
    return(bottom_temp_glorys)
  }

  # metadata ----
  attr(bottom_temp_glorys, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/bottom-temperatures.html"
  attr(bottom_temp_glorys, "data_files")   <- list(
    bottom_temp_glorys_csv = bottom_temp_glorys_csv)
  attr(bottom_temp_glorys, "data_steward") <- c(
    "Joseph Caracappa <joseph.caracappa@noaa.gov>")
}
get_bottom_temp_glorys(save_clean = T)
