### Thermal Habitat Area - Joe Caracappa

#library(dplyr)
#library(usethis)

get_thermal_habitat_area <- function(save_clean = F){

  thermal_habitat_area<-readr::read_csv(here::here("data-raw/thermal_habitat_area_2025.csv"),
                       show_col_types = F) |>
    dplyr::relocate(Time,EPU,Depth,Var,Value,Source)

  thermal_habitat_area$Depth <- factor(thermal_habitat_area$Depth, levels = c('0-25m','25-100m','100-3000m','AllDepths'))


  if (save_clean){
    usethis::use_data(thermal_habitat_area, overwrite = T)
  } else {
    return(thermal_habitat_area)
  }
}
get_thermal_habitat_area(save_clean = T)



