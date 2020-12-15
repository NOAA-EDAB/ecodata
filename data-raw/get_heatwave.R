#devtools::install_github("robwschlegel/heatwaveR")
library(heatwaveR)
library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")

heatwave_gb_csv<-"GB_OISST.csv"
heatwave_gom_csv<-"GOM_OISSt.csv"
heatwave_mab_csv<-"MAB_OISST.csv"

get_heatwave <- function(save_clean = F){

  gom<-read_csv(file.path(raw.dir,heatwave_gom_csv),
                 col_types = cols(temp = col_double(),t = col_date()))
  gb<-read_csv(file.path(raw.dir,heatwave_gb_csv),
                col_types = cols(temp = col_double(),t = col_date()))
  mab<-read_csv(file.path(raw.dir,heatwave_mab_csv),
                col_types = cols(temp = col_double(),t = col_date()))
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

  heatwave<- rbind(cum.intensity, max.intensity) %>%
  dplyr:: mutate(Units = "degrees C",
            Time = as.numeric(Time))

  if (save_clean){
    usethis::use_data(heatwave, overwrite = T)
  } else {
    return(heatwave)
  }
  # metadata ----
  attr(heatwave, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/marine-heatwave.html"
  attr(heatwave, "data_files")   <- list(
    heatwave_gom_csv = heatwave_gom_csv,
    heatwave_gb_csv = heatwave_gb_csv,
    heatwave_mab_csv = heatwave_mab_csv)
  attr(heatwave, "data_steward") <- c(
    "Vincent Saba <vincent.saba@noaa.gov>")
}
get_heatwave(save_clean = T)









#### get_heatwave_year get single year of heatwave
get_heatwave_year <- function(save_clean = F){
  # import data
  gom<-read_csv(file.path(raw.dir,"GOM_OISST.csv"),
                col_types = cols(temp = col_double(),t = col_date()))
  gb<-read_csv(file.path(raw.dir,"GB_OISST.csv"),
               col_types = cols(temp = col_double(),t = col_date()))
  mab<-read_csv(file.path(raw.dir,"MAB_OISST.csv"),
                col_types = cols(temp = col_double(),t = col_date()))
  #GB
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  gb.mhw <- heatwaveR::detect_event(ts)
  #GOM
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  gom.mhw <- heatwaveR::detect_event(ts)
  #MAB
  ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  mab.mhw <- heatwaveR::detect_event(ts)

  ### Take just clim
  #GB
  mhw<- gb.mhw$clim %>%
    mutate(EPU = c("GB"))# add EPU column
  mhw.gb.year <- mhw[13880:14223,]## days in 2020 data set only went to dec 9, 2020
  #GOM
  mhw<- gom.mhw$clim %>%
    mutate(EPU = c("GOM"))# add EPU column
  mhw.gom.year <- mhw[13880:14223,]## days in 2020 data set only went to dec 9, 2020
  #MAB
  mhw<- mab.mhw$clim %>%
    mutate(EPU = c("MAB"))# add EPU column
  mhw.mab.year <- mhw[13880:14223,]## days in 2020 data set only went to dec 9, 2020
#  mhw.mab.year2018 <- mhw[13150:13514,] Running 2018 for Kim H
  # bind dfs together for master list for plotting
  heatwave_year<- rbind(mhw.gb.year, mhw.gom.year, mhw.mab.year)
if (save_clean){
  usethis::use_data(heatwave_year, overwrite = T)
} else {
  return(heatwave_year)
}
}
get_heatwave_year(save_clean = T)


