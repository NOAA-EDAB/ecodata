#devtools::install_github("robwschlegel/heatwaveR")
library(heatwaveR)
library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")

## Surface Detrended
heatwave_gbd<-"GB_SST_1982_to_2024_detrended.csv"
heatwave_gomd<-"GOM_SST_1982_to_2024_detrended.csv"
heatwave_mabd<-"MAB_SST_1982_to_2024_detrended.csv"

## Bottom Detrended
bheatwave_gbd<-"daily_bottomT_GB_1959_2024_detrended.csv"
bheatwave_gomd<-"daily_bottomT_GOM_1959_2024_detrended.csv"
bheatwave_mabd<-"daily_bottomT_MAB_1959_2024_detrended.csv"

get_heatwave <- function(save_clean = F){

  # SURFACE DETRENDED
  # Process input files
  gom<-read.csv(file.path(raw.dir,heatwave_gomd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(t, detrended) %>%
    dplyr::rename(temp = detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  gb<-read.csv(file.path(raw.dir,heatwave_gbd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(t, detrended) %>%
    dplyr::rename(temp = detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  mab<-read.csv(file.path(raw.dir,heatwave_mabd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(t, detrended) %>%
    dplyr::rename(temp = detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  # GB - define climatology, detect events
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2024-12-31"))
  gb.mhw <- heatwaveR::detect_event(ts)
  gb.hw<- gb.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative)%>%
    dplyr::mutate(EPU = "GB")

  # GOM - define climatology, detect events
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2024-12-31"))
  gom.mhw <- heatwaveR::detect_event(ts)
  gom.hw<- gom.mhw$event %>%
   dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
   dplyr::mutate(EPU = "GOM")

  # MAB - define climatology, detect events
  ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2024-12-31"))
  mab.mhw <- heatwaveR::detect_event(ts)
  mab.hw<- mab.mhw$event %>%
   dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
   dplyr::mutate(EPU = "MAB")

  # Cumulative intensity
  cum.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
    dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")),
                  intensity_cumulative = as.numeric(intensity_cumulative)) %>%
    dplyr::group_by(Time, EPU) %>%
    dplyr::summarise(Value = as.numeric(sum(intensity_cumulative))) %>%
    dplyr::mutate(Var = "cumulative intensity") %>%
    dplyr::ungroup()

  # Max intensity
  max.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
    dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")),
                  intensity_max = as.numeric(intensity_max))  %>%
    dplyr::rename(Value = intensity_max) %>%
    dplyr::mutate(Var = "maximum intensity")%>%
    dplyr::select(Time, EPU, Value, Var)%>%
    dplyr::group_by(Time, EPU, Var) %>%
    dplyr::summarise(Value = max(Value)) %>%
    dplyr::ungroup()

  # Duration
  duration <- rbind(gb.hw, gom.hw, mab.hw) %>%
    dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")),
                  duration = as.numeric(duration))  %>%
    dplyr::rename(Value = duration) %>%
    dplyr::mutate(Var = "duration")%>%
    dplyr::select(Time, EPU, Value, Var) %>%
    dplyr::group_by(Time, EPU, Var) %>%
    dplyr::summarise(Value = max(Value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Units = "N days",
                  Time = as.numeric(Time),
                  Var  = paste0(Var, "-SurfaceDetrended"))

  # Create SOE data object
  heatwave_detrended<- rbind(cum.intensity, max.intensity) %>%
    dplyr:: mutate(Units = "degrees C",
                   Time = as.numeric(Time),
                   Var  = paste0(Var, "-SurfaceDetrended")) %>%
    rbind(duration) %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  # BOTTOM DETRENDED
  # Process input files
  gom<-read.csv(file.path(raw.dir,bheatwave_gomd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  gb<-read.csv(file.path(raw.dir,bheatwave_gbd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  mab<-read.csv(file.path(raw.dir,bheatwave_mabd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  # GB - define climatology, detect events
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2024-11-26"))
  gb.mhw <- heatwaveR::detect_event(ts, minDuration = 30)
  gb.hw<- gb.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative)%>%
    dplyr::mutate(EPU = "GB")

  # GOM - define climatology, detect events
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2024-11-26"))
  gom.mhw <- heatwaveR::detect_event(ts, minDuration = 30)
  gom.hw<- gom.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
    dplyr::mutate(EPU = "GOM")

  # MAB - define climatology, detect events
  ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2024-11-26"))
  mab.mhw <- heatwaveR::detect_event(ts, minDuration = 30)
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

  # Max intensity
  max.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
    dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")))  %>%
    dplyr::rename(Value = intensity_max) %>%
    dplyr::mutate(Var = "maximum intensity")%>%
    dplyr::select(Time, EPU, Value, Var)%>%
    dplyr::group_by(Time, EPU, Var) %>%
    dplyr::summarise(Value = max(Value)) %>%
    dplyr::ungroup()

  # Duration
  duration <- rbind(gb.hw, gom.hw, mab.hw) %>%
    dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")))  %>%
    dplyr::rename(Value = duration) %>%
    dplyr::mutate(Var = "duration")%>%
    dplyr::select(Time, EPU, Value, Var) %>%
    dplyr::group_by(Time, EPU, Var) %>%
    dplyr::summarise(Value = max(Value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Units = "N days",
                  Time = as.numeric(Time),
                  Var  = paste0(Var, "-BottomDetrended"))

  # Create SOE data object
  bheatwave_detrended<- rbind(cum.intensity, max.intensity) %>%
    dplyr:: mutate(Units = "degrees C",
                   Time = as.numeric(Time),
                   Var  = paste0(Var, "-BottomDetrended")) %>%
    rbind(duration) %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  # Merge surface and bottom SOE data object
  heatwave<- rbind(heatwave_detrended,
                   bheatwave_detrended)

  # Expand grid and join to dataset
  heatwave_zeros <- expand.grid(Time = unique(heatwave$Time),
                                Var = unique(heatwave$Var),
                                EPU = unique(heatwave$EPU)) %>%
    dplyr::mutate(Value2 = 0)

  heatwave<- heatwave %>% right_join(heatwave_zeros) %>%
    dplyr::mutate(Value = case_when(is.na(Value)~Value2,
                                    TRUE ~ Value)) %>%
    dplyr::select(!Value2)

  if (save_clean){
    usethis::use_data(heatwave, overwrite = T)
  } else {
    return(heatwave)
  }
}

get_heatwave(save_clean = T)







#### get_heatwave_year get single year of heatwave
get_heatwave_year <- function(save_clean = F){
  # import data
  # SURFACE DETRENDED
  gom<-read.csv(file.path(raw.dir,heatwave_gomd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(t, detrended) %>%
    dplyr::rename(temp = detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  gb<-read.csv(file.path(raw.dir,heatwave_gbd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(t, detrended) %>%
    dplyr::rename(temp = detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  mab<-read.csv(file.path(raw.dir,heatwave_mabd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(t, detrended) %>%
    dplyr::rename(temp = detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  #GB
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2024-12-31"))
  gb.mhw <- heatwaveR::detect_event(ts)

  #GOM
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2024-12-31"))
  gom.mhw <- heatwaveR::detect_event(ts)

  #MAB
  ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2024-12-31"))
  mab.mhw <- heatwaveR::detect_event(ts)

  ### Take just clim
  #GB
  mhw<- gb.mhw$clim %>%
    mutate(EPU = c("GB"),
           Year = c("2024"))# add EPU column
  mhw.gb.year <- mhw[15341:15706,]## days in 2024 data set

  #GOM
  mhw<- gom.mhw$clim %>%
    mutate(EPU = c("GOM"),
           Year = c("2024"))# add EPU column
  mhw.gom.year <- mhw[15341:15706,]## days in 2024 data set

  #MAB
  mhw<- mab.mhw$clim %>%
    mutate(EPU = c("MAB"),
           Year = c("2024"))# add EPU column
  mhw.mab.year <- mhw[15341:15706,]## days in 2024 data set


  heatwave_year_detrended<- rbind(mhw.gb.year, mhw.gom.year, mhw.mab.year) %>%
    dplyr::mutate(Var = "SurfaceDetrended")

  #Bottom heatwave detrended
  gom<-read.csv(file.path(raw.dir,bheatwave_gomd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  gb<-read.csv(file.path(raw.dir,bheatwave_gbd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  mab<-read.csv(file.path(raw.dir,bheatwave_mabd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  #GB
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2024-11-26"))
  gb.mhw <- heatwaveR::detect_event(ts, minDuration = 30)

  #GOM
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2024-11-26"))
  gom.mhw <- heatwaveR::detect_event(ts, minDuration = 30)

  #MAB
  ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2024-11-26"))
  mab.mhw <- heatwaveR::detect_event(ts, minDuration = 30)

  ### Take just clim
  #GB
  mhw<- gb.mhw$clim %>%
    mutate(EPU = c("GB"),
           Year = c("2024"))# add EPU column
  mhw.gb.year <- mhw[15341:15671,]## days in 2024 data set only went to Sep 10, 2024

  #GOM
  mhw<- gom.mhw$clim %>%
    mutate(EPU = c("GOM"),
           Year = c("2024"))# add EPU column
  mhw.gom.year <- mhw[15341:15671,]## days in 2024 data set only went to Sep 10, 2024

  #MAB
  mhw<- mab.mhw$clim %>%
    mutate(EPU = c("MAB"),
           Year = c("2024"))# add EPU column
  mhw.mab.year <- mhw[15341:15671,]## days in 2024 data set only went to Sep 10, 2024


  bheatwave_year_detrended<- rbind(mhw.gb.year, mhw.gom.year, mhw.mab.year) %>%
    dplyr::mutate(Var = "BottomDetrended")

  heatwave_year <- rbind(heatwave_year_detrended,
                         bheatwave_year_detrended)

if (save_clean){
  usethis::use_data(heatwave_year, overwrite = T)
} else {
  return(heatwave_year)
}
}
get_heatwave_year(save_clean = T)

## Old code, needs review and removal

# SURFACE
# gom<-read.csv(file.path(raw.dir,heatwave_gom), header = FALSE) %>%
#   dplyr::rename(V1, "t" ="V1",
#                 V2, "temp" ="V2") %>%
#   dplyr::filter(!temp == "temp") %>%
#   dplyr::mutate(temp = as.numeric(temp),
#                 t = as.Date(t, "%m/%d/%y")) %>%
#   tidyr::drop_na()
# gb<-read.csv(file.path(raw.dir,heatwave_gb), header = FALSE) %>%
#   dplyr::rename(V1, "t" ="V1",
#                 V2, "temp" ="V2") %>%
#   dplyr::filter(!temp == "temp") %>%
#   dplyr::mutate(temp = as.numeric(temp),
#                 t = as.Date(t, "%m/%d/%y")) %>%
#   tidyr::drop_na()
# mab<-read.csv(file.path(raw.dir,heatwave_mab), header = FALSE) %>%
#   dplyr::rename(V1, "t" ="V1",
#                 V2, "temp" ="V2") %>%
#   dplyr::filter(!temp == "temp") %>%
#   dplyr::mutate(temp = as.numeric(temp),
#                 t = as.Date(t, "%m/%d/%y")) %>%
#   tidyr::drop_na()
# #GB
# #ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2011-12-31"))
# ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2022-12-05"))
# gb.mhw <- heatwaveR::detect_event(ts)
# gb.hw<- gb.mhw$event %>%
#   dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative)%>%
#   dplyr::mutate(EPU = "GB")
# #GOM
# #ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
# ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2022-12-05"))
# gom.mhw <- heatwaveR::detect_event(ts)
# gom.hw<- gom.mhw$event %>%
#   dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
#   dplyr::mutate(EPU = "GOM")
# # MAB
# #ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2011-12-31"))
# ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2022-12-05"))
# mab.mhw <- heatwaveR::detect_event(ts)
# mab.hw<- mab.mhw$event %>%
#   dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
#   dplyr::mutate(EPU = "MAB")
# # Cumulative intensity
# cum.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
#   dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y"))) %>%
#   dplyr::group_by(Time, EPU) %>%
#   dplyr::summarise(Value = as.numeric(sum(intensity_cumulative))) %>%
#   dplyr::mutate(Var = "cumulative intensity") %>%
#   dplyr::ungroup()
# #Max intensity
# max.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
#   dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")))  %>%
#   dplyr::rename(Value = intensity_max) %>%
#   dplyr::mutate(Var = "maximum intensity")%>%
#   dplyr::select(Time, EPU, Value, Var) %>%
#   dplyr::group_by(Time, EPU, Var) %>%
#   dplyr::summarise(Value = max(Value)) %>%
#   dplyr::ungroup()
# duration <- rbind(gb.hw, gom.hw, mab.hw) %>%
#   dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")))  %>%
#   dplyr::rename(Value = duration) %>%
#   dplyr::mutate(Var = "duration")%>%
#   dplyr::select(Time, EPU, Value, Var) %>%
#   dplyr::group_by(Time, EPU, Var) %>%
#   dplyr::summarise(Value = max(Value)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(Units = "N days",
#                 Time = as.numeric(Time),
#                 Var  = paste0(Var, "-Surface"))
#
#
# heatwave_surface<- rbind(cum.intensity, max.intensity) %>%
# dplyr:: mutate(Units = "degrees C",
#           Time = as.numeric(Time),
#           Var  = paste0(Var, "-Surface")) %>%
#   rbind(duration) %>%
#   dplyr::select(Time, Var, Value, EPU, Units)
