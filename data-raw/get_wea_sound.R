#### Cetacean Acoustic Presence Indicator

library(dplyr)
library(tidyr)
library(readxl)

# Set directory
raw.dir <- here::here("data-raw")

# Define input files
wea_sound_xlsx <- "DecidecadeSPL_annual_SOE202410_JM - Jessica McCordic - NOAA Federal.xlsx"

get_wea_sound <- function(save_clean = F){

  wea_sound <- readxl::read_excel(file.path(raw.dir, wea_sound_xlsx)) |>
    tidyr::pivot_longer(cols = -c(Year, Measure, Site), names_to = "Var", values_to = "Value") |>
    dplyr::mutate(Var = paste0(Var, "-", Measure, "-", Site)) |>
    dplyr::select(-Measure, -Site) |>
    dplyr::rename("Time" = "Year")

  if (save_clean){
    usethis::use_data(wea_sound, overwrite = T)
  } else {
    return(wea_sound)
  }
}
get_wea_sound(save_clean = T)
