#### Cetacean Acoustic Presence Indicator

library(dplyr)
library(tidyr)

# Set directory
raw.dir <- here::here("data-raw")

# Define input files
# cetacean_acoustic_csv <- "VanHoeck_cetacean_acoustic_presence_all_sp_dataframe - Rebecca Vanhoeck - NOAA Federal.csv"
cetacean_acoustic_csv <- "PAM-cetacean-makara-export-nefsc-archival-20260729 - Rebecca Vanhoeck - NOAA Federal.csv"

get_cetacean_acoustic <- function(save_clean = F) {
  cetacean_acoustic <- read.csv(file.path(raw.dir, cetacean_acoustic_csv))

  cetacean_acoustic <- cetacean_acoustic |>
    dplyr::filter(recording_usable_start_datetime >= "2020-01-01") |>
    dplyr::filter(
      project_code %in%
        c(
          "PMEL_SBNMS-NRS",
          "NEFSC_SBNMS_SANCTSOUND",
          "NEFSC_SBNMS_ONMS-SOUND",
          "NEFSC_DE",
          "NEFSC_VA",
          "NEFSC_GOM_INSHORE",
          "NEFSC_GOM_OFFSHORE",
          "NEFSC_MA-RI_COX-LEDGE",
          "NEFSC_MA-RI_NANTUCKET-SHOALS",
          "NEFSC_MA-RI_POWERON"
        )
    ) |>
    # dplyr::mutate(end = end + lubridate::days(1)) |>
    # dplyr::select(-detectionType) |>
    dplyr::filter(presence != "NOT_DETECTED") |>
    dplyr::mutate(
      region = dplyr::case_when(
        project_code == "PMEL_SBNMS-NRS" ~ "SBNMS",
        project_code == "NEFSC_SBNMS_SANCTSOUND" ~ "SBNMS",
        project_code == "NEFSC_SBNMS_ONMS-SOUND" ~ "SBNMS",
        project_code == "NEFSC_DE" ~ "Mid-Atlantic",
        project_code == "NEFSC_VA" ~ "Mid-Atlantic",
        project_code == "NEFSC_GOM_INSHORE" ~ "Inshore GOM",
        project_code == "NEFSC_GOM_OFFSHORE" ~ "Offshore GOM",
        project_code == "NEFSC_MA-RI_COX-LEDGE" ~ "MA-RI",
        project_code == "NEFSC_MA-RI_NANTUCKET-SHOALS" ~ "MA-RI",
        project_code == "NEFSC_MA-RI_POWERON" ~ "MA-RI",
      )
    )

  if (save_clean) {
    usethis::use_data(cetacean_acoustic, overwrite = T)
  } else {
    return(cetacean_acoustic)
  }
}
get_cetacean_acoustic(save_clean = T)
