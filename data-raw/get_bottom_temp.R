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
    dplyr::mutate(Units = "degreesC", Time = as.Date(format(lubridate::date_decimal(Time), "%Y-%b-%d"), "%Y-%b-%d"),
           Var, Var = plyr::mapvalues(Var, from = c("Tsfc_anom",#Rename variables
                                                    "Tsfc_ref",
                                                    "Tbot_anom",
                                                    "Tbot_ref"),
                                      to = c("sst anomaly in situ",
                                             "reference sst in situ (1981-2010)",
                                             "bottom temp anomaly in situ",
                                             "reference bt in situ (1981-2010)"))) %>%
    dplyr::group_by(Time = lubridate::year(Time), EPU, Var, Units) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    as.data.frame()%>%
    tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  # metadata ----
  attr(bottom_temp, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/bottom-temperatures.html"
  attr(bottom_temp, "data_files")   <- list(
    bottom_temp_SS_csv = bottom_temp_SS_csv,
    bottom_temp_GB_csv = bottom_temp_GB_csv,
    bottom_temp_GOM_csv = bottom_temp_GOM_csv,
    bottom_temp_MAB_csv = bottom_temp_MAB_csv)
  attr(bottom_temp, "data_steward") <- c(
    "Paula Fratantoni <paula.fratantoni@noaa.gov>")
  attr(bottom_temp, "plot_script") <- list(
    `ltl_MAB` = "LTL_MAB.Rmd-bottom-temp.R",
    `ltl_NE` = "LTL_NE.Rmd-bottom-temp.R",
    `ltl_NE_gb` = "LTL_NE.Rmd-bottom-temp-gb.R",
    `ltl_NE_gom` = "LTL_NE.Rmd-bottom-temp-gom.R")

  if (save_clean){
    usethis::use_data(bottom_temp, overwrite = T)
  } else {
    return(bottom_temp)
  }
}
get_bottom_temp(save_clean = T)






#### GLORYS Data

bottom_temp_glorys_csv<-"GLORYS12v1_bottom_temp_anomaly_EPU - Joseph Caracappa - NOAA Affiliate.csv"


get_bottom_temp_glorys <- function(save_clean = F){

  bottom_temp_glorys <- read.csv(file.path(raw.dir,bottom_temp_glorys_csv)) %>%
    tibble::as_tibble()

  if (save_clean){
    usethis::use_data(bottom_temp_glorys, overwrite = T)
  } else {
    return(bottom_temp_glorys)
  }

  # metadata ----
  attr(bottom_temp_glorys, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/bottom-temperature---glorys.html"
  attr(bottom_temp_glorys, "data_files")   <- list(
    bottom_temp_glorys_csv = bottom_temp_glorys_csv)
  attr(bottom_temp_glorys, "data_steward") <- c(
    "Joseph Caracappa <joseph.caracappa@noaa.gov>")
}
get_bottom_temp_glorys(save_clean = T)










bottom_heatwave_xlsx<-"GB_GOM_MAB_BT_1959_2019 - Vincent Saba - NOAA Federal.xlsx"

get_bottom_heatwave <- function(save_clean = F){

  gom <- read_excel(file.path(raw.dir,bottom_heatwave_xlsx)) %>%
    dplyr::select(t, GOM_btemp) %>%
    dplyr::rename(temp = GOM_btemp) %>%
    dplyr::mutate(t = as.Date(t))
  gb <- read_excel(file.path(raw.dir,bottom_heatwave_xlsx)) %>%
    dplyr::select(t, GB_btemp) %>%
    dplyr::rename(temp = GB_btemp)%>%
    dplyr::mutate(t = as.Date(t))
  mab <- read_excel(file.path(raw.dir,bottom_heatwave_xlsx)) %>%
    dplyr::select(t, MAB_btemp) %>%
    dplyr::rename(temp = MAB_btemp)%>%
    dplyr::mutate(t = as.Date(t))
  #GB
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  gb.mhw <- heatwaveR::detect_event(ts)
  gb.hw<- gb.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative)%>%
    dplyr::mutate(EPU = "GB")
  #GOM
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  gom.mhw <- heatwaveR::detect_event(ts)
  gom.hw<- gom.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
    dplyr::mutate(EPU = "GOM")
  # MAB
  ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  mab.mhw <- heatwaveR::detect_event(ts)
  mab.hw<- mab.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
    dplyr::mutate(EPU = "MAB")
  # Cumulative intensity
  cum.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
    dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y"))) %>%
    dplyr::group_by(Time, EPU) %>%
    dplyr::summarise(Value = as.numeric(sum(intensity_cumulative))) %>%
    dplyr::mutate(Var = "cumulative intensity") %>%
    dplyr::ungroup()
  #Max intensity
  max.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
    dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")))  %>%
    dplyr::rename(Value = intensity_max) %>%
    dplyr::mutate(Var = "maximum intensity")%>%
    dplyr::select(Time, EPU, Value, Var)

  bottom_heatwave<- rbind(cum.intensity, max.intensity) %>%
    dplyr:: mutate(Units = "degrees C",
                   Time = as.numeric(Time)) %>%
    group_by(EPU) %>%
    complete(Var, Time = 1959:2019, fill = list(Value = 0))

  # metadata ----
  attr(bottom_heatwave, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/marine-heatwave.html"
  attr(heatwave, "data_files")   <- list(
    bottom_heatwave_xl = bottom_heatwave_xlsx)
  attr(heatwave, "data_steward") <- c(
    "Vincent Saba <vincent.saba@noaa.gov>")

  if (save_clean){
    usethis::use_data(bottom_heatwave, overwrite = T)
  } else {
    return(bottom_heatwave)
  }
}
get_bottom_heatwave(save_clean = T)


