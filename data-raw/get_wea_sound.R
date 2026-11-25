#### Ambient Sounds Levels in WEAs

library(dplyr)
library(tidyr)
library(readxl)

# Set directory
raw.dir <- here::here("data-raw")

# Define input files
wea_sound_csv <- "ambient_noise_for_SOE.csv"

get_wea_sound <- function(save_clean = F) {
  wea_sound <- read.csv(file.path(raw.dir, wea_sound_csv)) |>
    dplyr::mutate(
      Time = lubridate::as_date(Date),
      Var = paste(location, Category, sep = "_"),
      Units = "Broadband SPL 10Hz - 10kHz"
    ) |>
    dplyr::select(-c(Date, X, platform, year, effortS, Category, location)) |>
    dplyr::rename(Value = med_BB_10_10K) |>
    tibble::as_tibble()

  if (save_clean) {
    usethis::use_data(wea_sound, overwrite = T)
  } else {
    return(wea_sound)
  }
}
get_wea_sound(save_clean = T)
