### Thermal Habitat Area - Joe Caracappa

get_thermal_habitat_area <- function(save_clean = F){

  thermal_habitat_area <- readRDS(here::here("data-raw/GLORYS_thermal_habitat_area_1993_2025.rds")) |>
    dplyr::relocate(Time,EPU,Depth,Var,Value,Source)

  thermal_habitat_area$Depth <- factor(thermal_habitat_area$Depth, levels = c('0-25m','25-100m','100-300m','AllDepths'))


  if (save_clean){
    usethis::use_data(thermal_habitat_area, overwrite = T)
  } else {
    return(thermal_habitat_area)
  }
}
get_thermal_habitat_area(save_clean = T)
