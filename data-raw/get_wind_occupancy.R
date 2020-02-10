library(tidyverse)

raw.dir <- here::here("data-raw")

get_wind_occupancy <- function(save_clean = F){

  wind_occupancy <- read.csv(file.path(raw.dir, "wind-occupancy-1.csv"))

  if (save_clean){
    usethis::use_data(wind_occupancy, overwrite = T)
  } else {
    return(wind_occupancy)
  }

}
get_wind_occupancy(save_clean = T)
