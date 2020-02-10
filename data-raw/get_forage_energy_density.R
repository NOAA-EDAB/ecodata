library(tidyverse)

raw.dir <- here::here("data-raw")

get_forage_energy_density <- function(save_clean = F){

  energy_density <- read.csv(file.path(raw.dir, "forage.csv"))

  if (save_clean){
    usethis::use_data(energy_density, overwrite = T)
  } else {
    return(energy_density)
  }

}
get_forage_energy_density(save_clean = T)
