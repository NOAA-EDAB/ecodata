### Thermal Habitat Area - Joe Caracappa

# library(dplyr)
# library(usethis)

get_thermal_habitat_area_annual <- function(save_clean = F){

  thermal_habitat_area_annual<-readr::read_csv(here::here("data-raw/thermal_threshold_annual_area_SOE2025.csv"),
                       show_col_types = F) |>
    dplyr::relocate(Time,EPU,Var,Value,Source)

  if (save_clean){
    usethis::use_data(thermal_habitat_area_annual, overwrite = T)
  } else {
    return(thermal_habitat_area_annual)
  }
}
get_thermal_habitat_area_annual(save_clean = T)



