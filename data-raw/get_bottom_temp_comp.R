## Annual Bottom Temp
## Seasonal values from duPontavice gridded model

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
bt_csv <- "bt_temp_product_epu - Hubert duPontavice - NOAA Affiliate.csv"
bt_seasonal_csv<- "bt_temp_times_series_anomaly_epu.csv"
get_bottom_temp_comp <- function(save_clean = F){

  bottom_temp_comp<- read.csv(file.path(raw.dir,bt_csv))  %>%
    dplyr::select(!X) %>%
    dplyr::rename("Time" = "year",
                  "EPU" = "subarea",
                  "Value" = "bt_temp") %>%
    dplyr::mutate(Var = c("Annual_Bottom Temp"),
                  Units = c("degree C"))
  bottom_temp_comp_seasonal<- read.csv(file.path(raw.dir,bt_seasonal_csv))  %>%
    dplyr::select(!X) %>%
    dplyr::mutate(season = recode(season, "1" = "Winter",
                                  "2" = "Spring", "3" = "Summer",
                                  "4" = "Fall")) %>%
    dplyr::rename("Time" = "year",
                  "EPU" = "subarea",
                  "Value" = "anomaly") %>%
    dplyr::mutate(Var = paste0(season,"_Bottom Temp Anomaly"),
                  Units = c("degree C")) %>%
    dplyr::select(Time, Value, Var, EPU, Units)

  bottom_temp_comp<- rbind(bottom_temp_comp, bottom_temp_comp_seasonal) %>%
    tibble::as_tibble()

  if (save_clean){
    usethis::use_data(bottom_temp_comp, overwrite = T)
  } else {
    return(bottom_temp_comp)
  }
}
get_bottom_temp_comp(save_clean = T)


bt_nc <- "bt_temp_anomaly_season_2022_soe.nc"

get_bottom_temp_seasonal_gridded <- function(save_clean = F){

  b1<- raster::stack(file.path(raw.dir,bt_nc))
  seasonal_bt_anomaly_gridded<- as.data.frame(b1) %>%
    cbind(coordinates(b1)) %>%
    dplyr::rename("Winter" = X1,
                  "Spring" = X2,
                  "Summer" = X3,
                  "Fall" = X4,
                  "Latitude" = y,
                  "Longitude" = x) %>%
    tidyr::pivot_longer(cols = c(Winter,Spring,
                                 Summer, Fall),
                        names_to = "Season", values_to = "Value")

  if (save_clean){
    usethis::use_data(seasonal_bt_anomaly_gridded, overwrite = T)
  } else {
    return(seasonal_bt_anomaly_gridded)
  }
}
get_bottom_temp_seasonal_gridded(save_clean = T)

