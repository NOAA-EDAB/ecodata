### Thermal Habitat Area - Joe Caracappa

#library(dplyr)
#library(usethis)

get_thermal_habitat <- function(save_clean = F){

  thermal_habitat<-readr::read_csv(here::here("data-raw/thermal_habitat_area_2023 - Joseph Caracappa - NOAA Federal.csv"),
                       show_col_types = F) |>
    dplyr::mutate(Units = "Proportion",
                  date = lubridate::as_date(date),
                  Var = paste0(">",temp.threshold,"\u00B0C"),
                  Depth = paste0(min.depth,"-",max.depth,"m")) |>
    dplyr::select(-c(area, min.depth,max.depth)) |>
    dplyr::rename(EPU = epu,
                  Time = date,
                  Value = area.prop,
                  Source = source) |>
    dplyr::relocate(Time,EPU,Depth,Var,Value,Source)

  thermal_habitat$Depth <- factor(thermal_habitat$Depth, levels = c('0-25m','25-100m','100-3000m'))


  if (save_clean){
    usethis::use_data(thermal_habitat, overwrite = T)
  } else {
    return(thermal_habitat)
  }
}
get_thermal_habitat(save_clean = T)



