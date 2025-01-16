## Annual Bottom Temp
## Seasonal values from duPontavice gridded model

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
bt_csv <- "bottom_temp_anomaly_2025_V3.csv"
bottom_temp_mom6_csv <- "BT_m6_gl_2024_soe.csv - Laura Gruenburg - NOAA Federal.csv"

get_bottom_temp_model_anom <- function(save_clean = F){

  bottom_temp_model_anom <- read.csv(file.path(raw.dir,bt_csv))

  bottom_temp_mom6 <- read.csv(file.path(raw.dir, bottom_temp_mom6_csv)) |>
    dplyr::select(Time, Var, Value, Units, EPU, Source)

  bottom_temp_model_anom <- rbind(bottom_temp_model_anom, bottom_temp_mom6)

  if (save_clean){
    usethis::use_data(bottom_temp_model_anom, overwrite = T)
  } else {
    return(bottom_temp_model_anom)
  }
}
get_bottom_temp_model_anom(save_clean = T)


btsg_csv <- "bottom_temp_seasonal_gridded_2025.csv"

get_bottom_temp_model_gridded <- function(save_clean = F){

  bottom_temp_model_gridded<- read.csv(file.path(raw.dir,btsg_csv))

  if (save_clean){
    usethis::use_data(bottom_temp_model_gridded, overwrite = T)
  } else {
    return(bottom_temp_model_gridded)
  }
}
get_bottom_temp_model_gridded(save_clean = T)
