## Annual Bottom Temp
## Seasonal values from duPontavice gridded model

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
bt_csv <- "bt_temp_annual.csv"
bt_seasonal_csv<- "bt_temp_time_series_anomaly_epu - Joseph Caracappa - NOAA Federal.csv"
get_bottom_temp_comp <- function(save_clean = F){

  bottom_temp_comp<- read.csv(file.path(raw.dir,bt_csv))  %>%
    dplyr::rename("Time" = "year",
                  "EPU" = "subarea",
                  "Value" = "bt_temp",
                  "Source" = "source") %>%
    dplyr::mutate(Var = c("Annual_Bottom Temp"),
                  Units = c("degree C"))
  bottom_temp_comp_seasonal<- read.csv(file.path(raw.dir,bt_seasonal_csv))  %>%
    #dplyr::select(!X) %>%
    dplyr::mutate(season = recode(season, "1" = "Winter",
                                  "2" = "Spring", "3" = "Summer",
                                  "4" = "Fall")) %>%
    dplyr::rename("Time" = "year",
                  "EPU" = "subarea",
                  "Value" = "anomaly",
                  "Source" = "source") %>%
    dplyr::mutate(Var = paste0(season,"_Bottom Temp Anomaly"),
                  Units = c("degree C")) %>%
    dplyr::select(Time, Value, Var, EPU, Units, Source)

  bottom_temp_comp<- rbind(bottom_temp_comp, bottom_temp_comp_seasonal) %>%
    tibble::as_tibble()

  if (save_clean){
    usethis::use_data(bottom_temp_comp, overwrite = T)
  } else {
    return(bottom_temp_comp)
  }
}
get_bottom_temp_comp(save_clean = T)


btsg_csv <- "bt_seasonal_gridded.csv"

get_bottom_temp_seasonal_gridded <- function(save_clean = F){

  bottom_temp_seasonal_gridded<- read.csv(file.path(raw.dir,btsg_csv)) %>%
    dplyr::rename("Latitude" = "Lat",
                  "Longitude" = "Lon",
                  "Season" = "Variable")

  if (save_clean){
    usethis::use_data(bottom_temp_seasonal_gridded, overwrite = T)
  } else {
    return(bottom_temp_seasonal_gridded)
  }
}
get_bottom_temp_seasonal_gridded(save_clean = T)

