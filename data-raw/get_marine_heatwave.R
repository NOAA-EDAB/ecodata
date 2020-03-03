#devtools::install_github("robwschlegel/heatwaveR")
library(heatwaveR)
library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")


get_marine_heatwave <- function(save_clean = F){

  gom<-read_csv(file.path(raw.dir,"GOM_OISST - Vincent Saba - NOAA Federal.csv"),
                 col_types = cols(temp = col_double(),t = col_date()))
  gb<-read_csv(file.path(raw.dir,"GB_OISST - Vincent Saba - NOAA Federal.csv"),
                col_types = cols(temp = col_double(),t = col_date()))
  mab<-read_csv(file.path(raw.dir,"MAB_OISST - Vincent Saba - NOAA Federal.csv"),
                col_types = cols(temp = col_double(),t = col_date()))
  #GB
  ts <- ts2clm(gb, climatologyPeriod = c("1982-01-01", "2010-12-31"))
  gb.mhw <- detect_event(ts)
  gb.hw<- gb.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative)%>%
    mutate(EPU = "GB")
  #GOM
  ts <- ts2clm(gom, climatologyPeriod = c("1982-01-01", "2010-12-31"))
  gom.mhw <- detect_event(ts)
  gom.hw<- gom.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
    mutate(EPU = "GOM")
  # MAB
  ts <- ts2clm(mab, climatologyPeriod = c("1982-01-01", "2010-12-31"))
  mab.mhw <- detect_event(ts)
  mab.hw<- mab.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
    mutate(EPU = "MAB")
  # Cumulative intensity
  cum.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
    mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y"))) %>%
    group_by(Time, EPU) %>%
    summarise(Value = as.numeric(sum(intensity_cumulative))) %>%
    mutate(Var = "cumulative intensity") %>%
    ungroup
  #Max intensity
  max.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
    mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")))  %>%
    rename(Value = intensity_max) %>%
    mutate(Var = "maximum intensity")%>%
    dplyr::select(Time, EPU, Value, Var)

  heatwave<- rbind(cum.intensity, max.intensity) %>%
    mutate(Units = "degrees C",
           Time = as.numeric(Time))

  if (save_clean){
    usethis::use_data(heatwave, overwrite = T)
  } else {
    return(heatwave)
  }
}
get_marine_heatwave(save_clean = T)





# Plot from Vince https://drive.google.com/drive/u/0/folders/0B1lP7X3GP_hQfk00dFd6bTQ2cmJyQjlwQnFWQ0JMWF9ZOWhRckNvSTlxT2NHSEZRdjV1WVk
# #
# event_line(gom.mhw, spread = 200, metric = "intensity_cumulative",
#            start_date = "2019-01-01", end_date = "2019-12-08") ## Plot the year
#
#


##########################    Plotting for SOE   ######################
# mhw<- gom.mhw$clim ## coming from above, detect_event() from heatwaveR
#
# mhw.max.year <- mhw[13515:13857,]## days in 2019 data set only went to dec 9, 2019
#
# mhw.max.year %>%
#   ggplot( aes(x = t, y = temp))+
#   geom_flame(aes(y2 = thresh))+ #heatwaveR function
#   geom_line(aes(x = t, y = seas, color = "a"), size = 1)+
#   geom_line(aes(x = t, y = thresh, color = "c"), size = 1)+
#   geom_line(aes(x = t, y = temp, color = "b"))+
#   scale_colour_manual(values = c("turquoise4", "sienna3", "black"),
#                       labels = c("Climatology","Temperature", "Threshold"))+
#   ylab("Temperature (C)")+
#   xlab(element_blank())+
#   scale_x_date(date_labels = "%b", breaks = "1 month")+
#   theme_bw()+
#   theme(legend.title = element_blank(),
#         legend.position=c(0.2, 0.8))
