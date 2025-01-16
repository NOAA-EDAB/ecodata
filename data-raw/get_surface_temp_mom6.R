#### MOM6 Surface Temperature

library(dplyr)
library(tidyr)

# Set directory
raw.dir <- here::here("data-raw")

# Define input files
surface_temp_mom6_forecast_csv <- "tos_10forecast - Laura Gruenburg - NOAA Federal.csv"
surface_temp_mom6_hindcast_csv <- "tos_hindcast_1993_2019 - Laura Gruenburg - NOAA Federal.csv"

get_surface_temp_mom6 <- function(save_clean = F){

  surface_temp_mom6_forecast<- read.csv(file.path(raw.dir, surface_temp_mom6_forecast_csv)) |>
    dplyr::mutate(Var = c("forecast")) |>
    dplyr::mutate(Var = paste0(Var, "-",e_member)) |>
    dplyr::select(Time, Var, Val, Unit, EPU) |>
    dplyr::rename(Value = Val, Units = Unit)

  surface_temp_mom6_hindcast<- read.csv(file.path(raw.dir, surface_temp_mom6_hindcast_csv)) |>
    dplyr::mutate(Season = dplyr::recode(Season, "10" = "Fall",
                                         "1" = "Winter",
                                         "4" = "Spring",
                                         "7" = "Summer")) |>
    dplyr::mutate(Var = c("hindcast")) |>
    dplyr::mutate(Var = paste0(Season, "-",Var)) |>
    dplyr::rename(Units = Unit) |>
    dplyr::select(Time, Var, Value, Units, EPU)

  surface_temp_mom6<-rbind(surface_temp_mom6_forecast,
                           surface_temp_mom6_hindcast)

  if (save_clean){
    usethis::use_data(surface_temp_mom6, overwrite = T)
  } else {
    return(surface_temp_mom6)
  }
}
get_surface_temp_mom6(save_clean = T)
