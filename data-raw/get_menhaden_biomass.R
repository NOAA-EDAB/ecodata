# Process menhanden biomass data

# Time series of Age 1+ biomass from 2025 stock assessment, provided by ASMFC

library(dplyr)
library(tidyr)
library(readr)

raw.dir <- here::here("data-raw")
menhaden_csv <- "menhaden biomass - Abigail Tyrell - NOAA Federal.csv"

get_menhaden_biomass <- function(save_clean = F) {
  menhaden_biomass <- read.csv(
    here::here(file.path(raw.dir, menhaden_csv)),
    check.names = FALSE
  ) %>%
    dplyr::mutate(
      `Age-1+ Biomass (thousands of MT)` = readr::parse_number(
        `Age-1+ Biomass (thousands of MT)`
      )
    ) %>%
    dplyr::rename(
      "Time" = Year,
      "Value" = `Age-1+ Biomass (thousands of MT)`
    ) %>%
    dplyr::mutate(
      EPU = "All",
      Var = "Menhaden biomass",
      Units = "thousand mt"
    )

  if (save_clean) {
    usethis::use_data(menhaden_biomass, overwrite = TRUE)
  } else {
    return(menhaden_biomass)
  }
}
get_menhaden_biomass(save_clean = T)
