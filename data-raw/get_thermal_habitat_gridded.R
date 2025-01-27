### Thermal Habitat Persistence - Joe Caracappa

#library(dplyr)
#library(usethis)

# raw.dir <- here::here("data-raw")
# th_gridded_rda<- "thermal_habitat_gridded_2024_only_SOE2025.csv"

get_thermal_habitat_gridded <- function(save_clean = F){

    # load(file.path(raw.dir, th_gridded_rda))
    thermal_habitat_gridded <- read.csv(here::here('data-raw','thermal_habitat_gridded_2024_only_SOE2025.csv')) |>
    dplyr::relocate(Time,EPU,Depth,Var,Value,Latitude,Longitude,Source)

  #thermal_habitat_gridded$Depth <- factor(thermal_habitat_gridded$Depth, levels = c('0-25m','25-100m','100-3000m'))


  if (save_clean){
    usethis::use_data(thermal_habitat_gridded, overwrite = T)
  } else {
    return(thermal_habitat_gridded)
  }
}
get_thermal_habitat_gridded(save_clean = T)
