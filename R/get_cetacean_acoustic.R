#### Cetacean Acoustic Presence Indicator

library(dplyr)
library(tidyr)

# Set directory
raw.dir <- here::here("data-raw")

# Define input files
cetacean_acoustic_csv <- "VanHoeck_cetacean_acoustic_presence_all_sp_dataframe - Rebecca Vanhoeck - NOAA Federal.csv"

get_cetacean_acoustic <- function(save_clean = F){

  cetacean_acoustic <- read.csv(file.path(raw.dir, cetacean_acoustic_csv)) |>
    dplyr::select(-c(PROJECT_DESCRIPTION, DeploymentStart, Site, Full_Day, PROJECT_NAME, SITE_NAME,
                  START_USABLE_DATA_DATETIME_UTC, START_USABLE_DATE_EST, END_USABLE_DATA_DATETIME_UTC,
                  END_USABLE_DATE_EST, ITEM_DESCRIPTION, LATITUDE_DDG_DEPLOYMENT, LONGITUDE_DDG_DEPLOYMENT,
                  DataStart, DataEnd)) |>
    dplyr::mutate(NARW_Occur = as.character(NARW_Occur)) |>
    dplyr::mutate(NARW_True_Tally = as.character(NARW_True_Tally)) |>
    dplyr::mutate(Fin_Occur = as.character(Fin_Occur)) |>
    dplyr::mutate(Humpback_Occur = as.character(Humpback_Occur)) |>
    dplyr::mutate(Minke_Occur = as.character(Minke_Occur)) |>
    dplyr::mutate(Blue_Occur = as.character(Blue_Occur)) |>
    dplyr::mutate(Sei_Occur = as.character(Sei_Occur)) |>
    dplyr::mutate(Sperm_Occur = as.character(Sperm_Occur)) |>
    dplyr::mutate(Dolphin_Occur = as.character(Dolphin_Occur)) |>
    dplyr::mutate(Porpoise_Occur = as.character(Porpoise_Occur)) |>
    tidyr::pivot_longer(cols = c(-StartDate), names_to = "Var", values_to = "Value") |>
    dplyr::rename("Time" = "StartDate") |>
    dplyr::mutate_all(na_if, "")

  if (save_clean){
    usethis::use_data(cetacean_acoustic, overwrite = T)
  } else {
    return(cetacean_acoustic)
  }
}
get_cetacean_acoustic(save_clean = T)
