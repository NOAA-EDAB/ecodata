## Annual Bottom Temp
## Seasonal values from duPontavice gridded model

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
bt_csv <- "bt_temp_product_epu - Hubert duPontavice - NOAA Affiliate.csv"

get_bottom_temp_comp <- function(save_clean = F){

  bottom_temp_comp<- read.csv(file.path(raw.dir,bt_csv))  %>%
    dplyr::select(!X) %>%
    dplyr::rename("Time" = "year",
                  "EPU" = "subarea",
                  "Value" = "bt_temp") %>%
    dplyr::mutate(Var = c("Bottom Temp"),
                  Units = c("degree C"))

  if (save_clean){
    usethis::use_data(bottom_temp_comp, overwrite = T)
  } else {
    return(bottom_temp_comp)
  }
}
get_bottom_temp_comp(save_clean = T)


bt_nc <- "bt_temp_product_season_1959_2022 - Hubert duPontavice - NOAA Affiliate.nc"

get_bottom_temp_seasnonal_gridded <- function(save_clean = F){

  bottom_temp_seasonal_gridded<- raster::stack(file.path(raw.dir,bt_nc))

  if (save_clean){
    usethis::use_data(bottom_temp_seasonal_gridded, overwrite = T)
  } else {
    return(bottom_temp_seasonal_gridded)
  }
}
get_bottom_temp_seasnonal_gridded(save_clean = T)

