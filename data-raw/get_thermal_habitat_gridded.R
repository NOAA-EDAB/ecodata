### Thermal Habitat Persistence - Joe Caracappa

#library(dplyr)
#library(usethis)

get_thermal_habitat_gridded <- function(save_clean = F){

  thermal_habitat_gridded<-readr::read_csv(here::here("data-raw/thermal_habitat_persistence_2023 - Joseph Caracappa - NOAA Federal.csv"),
                       show_col_types = F) |>
    dplyr::mutate(Units = "Number of Days",
                  Depth = paste0(min.depth,"-",max.depth,"m"),
                  EPU = "All") |>
    dplyr::select(-c(min.depth,max.depth)) |>
    dplyr::rename(Time = year,
                  Value = Ndays,
                  Var = temp.threshold,
                  Source = source,
                  Latitude = latitude,
                  Longitude = longitude) |>
    dplyr::relocate(Time,EPU,Depth,Var,Value,Latitude,Longitude,Source)

  #thermal_habitat_gridded$Depth <- factor(thermal_habitat_gridded$Depth, levels = c('0-25m','25-100m','100-3000m'))


  if (save_clean){
    usethis::use_data(thermal_habitat_gridded, overwrite = T)
  } else {
    return(thermal_habitat_gridded)
  }
}
get_thermal_habitat_gridded(save_clean = T)



