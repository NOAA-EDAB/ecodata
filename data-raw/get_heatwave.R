#devtools::install_github("robwschlegel/heatwaveR")
library(heatwaveR)
library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")
## Surface
heatwave_gb<-"GB_SST_1982_to_2022.csv"
heatwave_gom<-"GOM_SST_1982_to_2022.csv"
heatwave_mab<-"MAB_SST_1982_to_2022.csv"

## Surface Detrended
heatwave_gbd<-"GB_SST_1982_to_2022_detrended.csv"
heatwave_gomd<-"GOM_SST_1982_to_2022_detrended.csv"
heatwave_mabd<-"MAB_SST_1982_to_2022_detrended.csv"

## Bottom Detrended
bheatwave_gbd<-"GB_BT_1982_to_2022_detrended.csv"
bheatwave_gomd<-"GOM_BT_1982_to_2022_detrended.csv"
bheatwave_mabd<-"MAB_BT_1982_to_2022_detrended.csv"

get_heatwave <- function(save_clean = F){

  # SURFACE
  gom<-read.csv(file.path(raw.dir,heatwave_gom), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, "%m/%d/%y")) %>%
    tidyr::drop_na()
  gb<-read.csv(file.path(raw.dir,heatwave_gb), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, "%m/%d/%y")) %>%
    tidyr::drop_na()
  mab<-read.csv(file.path(raw.dir,heatwave_mab), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, "%m/%d/%y")) %>%
    tidyr::drop_na()
  #GB
  #ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  gb.mhw <- heatwaveR::detect_event(ts)
  gb.hw<- gb.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative)%>%
    dplyr::mutate(EPU = "GB")
  #GOM
  #ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  gom.mhw <- heatwaveR::detect_event(ts)
  gom.hw<- gom.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
    dplyr::mutate(EPU = "GOM")
  # MAB
  #ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2011-12-31"))
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
    dplyr::select(Time, EPU, Value, Var) %>%
    dplyr::group_by(Time, EPU, Var) %>%
    dplyr::summarise(Value = max(Value)) %>%
    dplyr::ungroup()

  heatwave_surface<- rbind(cum.intensity, max.intensity) %>%
  dplyr:: mutate(Units = "degrees C",
            Time = as.numeric(Time),
            Var  = paste0(Var, "-Surface")) %>%
    dplyr::select(Time, Var, Value, EPU, Units)


  # SURFACE DETRENDED
  gom<-read.csv(file.path(raw.dir,heatwave_gomd), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, "%m/%d/%y")) %>%
    tidyr::drop_na()
  gb<-read.csv(file.path(raw.dir,heatwave_gbd), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, "%m/%d/%y")) %>%
    tidyr::drop_na()
  mab<-read.csv(file.path(raw.dir,heatwave_mabd), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, "%m/%d/%y")) %>%
    tidyr::drop_na()
  #GB
  #ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  gb.mhw <- heatwaveR::detect_event(ts)
  gb.hw<- gb.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative)%>%
    dplyr::mutate(EPU = "GB")
  #GOM
  #ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  gom.mhw <- heatwaveR::detect_event(ts)
  gom.hw<- gom.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
    dplyr::mutate(EPU = "GOM")
  # MAB
  #ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2011-12-31"))
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
    dplyr::select(Time, EPU, Value, Var)%>%
    dplyr::group_by(Time, EPU, Var) %>%
    dplyr::summarise(Value = max(Value)) %>%
    dplyr::ungroup()

  heatwave_detrended<- rbind(cum.intensity, max.intensity) %>%
    dplyr:: mutate(Units = "degrees C",
                   Time = as.numeric(Time),
                   Var  = paste0(Var, "-SurfaceDetrended")) %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  # BOTTOM DETRENDED
  gom<-read.csv(file.path(raw.dir,bheatwave_gomd), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t)) %>%
    tidyr::drop_na()
  gb<-read.csv(file.path(raw.dir,bheatwave_gbd), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t)) %>%
    tidyr::drop_na()
  mab<-read.csv(file.path(raw.dir,bheatwave_mabd), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t)) %>%
    tidyr::drop_na()
  #GB
  #ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  gb.mhw <- heatwaveR::detect_event(ts)
  gb.hw<- gb.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative)%>%
    dplyr::mutate(EPU = "GB")
  #GOM
  #ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  gom.mhw <- heatwaveR::detect_event(ts)
  gom.hw<- gom.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
    dplyr::mutate(EPU = "GOM")
  # MAB
  #ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2011-12-31"))
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
    dplyr::select(Time, EPU, Value, Var)%>%
    dplyr::group_by(Time, EPU, Var) %>%
    dplyr::summarise(Value = max(Value)) %>%
    dplyr::ungroup()

  bheatwave_detrended<- rbind(cum.intensity, max.intensity) %>%
    dplyr:: mutate(Units = "degrees C",
                   Time = as.numeric(Time),
                   Var  = paste0(Var, "-BottomDetrended")) %>%
    dplyr::select(Time, Var, Value, EPU, Units)


  heatwave<- rbind(heatwave_surface, heatwave_detrended,
                   bheatwave_detrended)

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





heatwave %>% filter(EPU == "GOM",
                    Var == "cumulative intensity-BottomDetrended") %>%
  ggplot(aes(x=Time, y = Value))+
  geom_point()+
  geom_line()

df %>% filter(EPU == "GOM",
                    Var == "cumulative intensity-BottomDetrended") %>%
  ggplot(aes(x=Time, y = Value))+
  geom_point()+
  geom_line()


#### get_heatwave_year get single year of heatwave
get_heatwave_year <- function(save_clean = F){
  # import data
  # SURFACE
  gom<-read.csv(file.path(raw.dir,heatwave_gom), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, "%m/%d/%y")) %>%
    tidyr::drop_na()
  gb<-read.csv(file.path(raw.dir,heatwave_gb), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, "%m/%d/%y")) %>%
    tidyr::drop_na()
  mab<-read.csv(file.path(raw.dir,heatwave_mab), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, "%m/%d/%y")) %>%
    tidyr::drop_na()
  #GB
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  #ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1991-01-01", "2020-12-31"))
  gb.mhw <- heatwaveR::detect_event(ts)
  #GOM
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  #ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1991-01-01", "2020-12-31"))
  gom.mhw <- heatwaveR::detect_event(ts)
  #MAB
  ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  #ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1991-01-01", "2020-12-31"))
  mab.mhw <- heatwaveR::detect_event(ts)

  ### Take just clim
  #GB
  mhw<- gb.mhw$clim %>%
    mutate(EPU = c("GB"),
           Year = c("2022"))# add EPU column
  mhw.gb.year <- mhw[14611:14949,]## days in 2020 data set only went to dec 9, 2020
  #GOM
  mhw<- gom.mhw$clim %>%
    mutate(EPU = c("GOM"),
           Year = c("2022"))# add EPU column
  mhw.gom.year <- mhw[14611:14949,]## days in 2020 data set only went to dec 9, 2020
  #MAB
  mhw<- mab.mhw$clim %>%
    mutate(EPU = c("MAB"),
           Year = c("2022"))# add EPU column
  mhw.mab.year <- mhw[14611:14949,]## days in 2020 data set only went to dec 9, 2020


  heatwave_year<- rbind(mhw.gb.year, mhw.gom.year, mhw.mab.year) %>%
    dplyr::mutate(Var = "Surface")

  # SURFACE DETRENDED
  gom<-read.csv(file.path(raw.dir,heatwave_gomd), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, "%m/%d/%y")) %>%
    tidyr::drop_na()
  gb<-read.csv(file.path(raw.dir,heatwave_gbd), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, "%m/%d/%y")) %>%
    tidyr::drop_na()
  mab<-read.csv(file.path(raw.dir,heatwave_mabd), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, "%m/%d/%y")) %>%
    tidyr::drop_na()

  #GB
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  #ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1991-01-01", "2020-12-31"))
  gb.mhw <- heatwaveR::detect_event(ts)
  #GOM
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  #ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1991-01-01", "2020-12-31"))
  gom.mhw <- heatwaveR::detect_event(ts)
  #MAB
  ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  #ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1991-01-01", "2020-12-31"))
  mab.mhw <- heatwaveR::detect_event(ts)

  ### Take just clim
  #GB
  mhw<- gb.mhw$clim %>%
    mutate(EPU = c("GB"),
           Year = c("2022"))# add EPU column
  mhw.gb.year <- mhw[14611:14949,]## days in 2020 data set only went to dec 9, 2020
  #GOM
  mhw<- gom.mhw$clim %>%
    mutate(EPU = c("GOM"),
           Year = c("2022"))# add EPU column
  mhw.gom.year <- mhw[14611:14949,]## days in 2020 data set only went to dec 9, 2020
  #MAB
  mhw<- mab.mhw$clim %>%
    mutate(EPU = c("MAB"),
           Year = c("2022"))# add EPU column
  mhw.mab.year <- mhw[14611:14949,]## days in 2020 data set only went to dec 9, 2020


  heatwave_year_detrended<- rbind(mhw.gb.year, mhw.gom.year, mhw.mab.year) %>%
    dplyr::mutate(Var = "SurfaceDetrended")


  gom<-read.csv(file.path(raw.dir,bheatwave_gomd), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t)) %>%
    tidyr::drop_na()
  gb<-read.csv(file.path(raw.dir,bheatwave_gbd), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t)) %>%
    tidyr::drop_na()
  mab<-read.csv(file.path(raw.dir,bheatwave_mabd), header = FALSE) %>%
    dplyr::rename(V1, "t" ="V1",
                  V2, "temp" ="V2") %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t)) %>%
    tidyr::drop_na()

  #GB
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  #ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1991-01-01", "2020-12-31"))
  gb.mhw <- heatwaveR::detect_event(ts)
  #GOM
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  #ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1991-01-01", "2020-12-31"))
  gom.mhw <- heatwaveR::detect_event(ts)
  #MAB
  ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  #ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1991-01-01", "2020-12-31"))
  mab.mhw <- heatwaveR::detect_event(ts)

  ### Take just clim
  #GB
  mhw<- gb.mhw$clim %>%
    mutate(EPU = c("GB"),
           Year = c("2022"))# add EPU column
  mhw.gb.year <- mhw[14611:14949,]## days in 2020 data set only went to dec 9, 2020
  #GOM
  mhw<- gom.mhw$clim %>%
    mutate(EPU = c("GOM"),
           Year = c("2022"))# add EPU column
  mhw.gom.year <- mhw[14611:14949,]## days in 2020 data set only went to dec 9, 2020
  #MAB
  mhw<- mab.mhw$clim %>%
    mutate(EPU = c("MAB"),
           Year = c("2022"))# add EPU column
  mhw.mab.year <- mhw[14611:14949,]## days in 2020 data set only went to dec 9, 2020


  bheatwave_year_detrended<- rbind(mhw.gb.year, mhw.gom.year, mhw.mab.year) %>%
    dplyr::mutate(Var = "BottomDetrended")

  heatwave_year <- rbind(heatwave_year, heatwave_year_detrended,
                         bheatwave_year_detrended)
if (save_clean){
  usethis::use_data(heatwave_year, overwrite = T)
} else {
  return(heatwave_year)
}
}
get_heatwave_year(save_clean = T)

