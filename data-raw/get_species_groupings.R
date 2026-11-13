# Processing species groupings

# See the following links for more information regarding the NEFSC ESB Bottom
# Trawl Survey, CFDBS, and ITIS:
# https://www.itis.gov/
# https://inport.nmfs.noaa.gov/inport/item/22561
# https://inport.nmfs.noaa.gov/inport/item/22560
# https://inport.nmfs.noaa.gov/inport/item/27401
#
# More information about the NE-IEA program is available here:
# http://integratedecosystemassessment.noaa.gov

raw.dir <- here::here("data-raw")

get_species_groupings <- function(save_clean = F) {
  ## last downloaded 2026-11-13, "updated new_species_groupings" sheet
  ## from: https://docs.google.com/spreadsheets/d/1Bbmi_GzFiWfOqABwzcGHwAA_uKwdDiKuGkVDYD6DYa8/edit?gid=1143921734#gid=1143921734
  species <- read.csv(here::here(raw.dir, "updated new_species_groupings.csv"))
  species_groupings <- species |>
    tibble::as_tibble() |>
    ## OVERWRITING SOE.24 WITH Guild as temp fix so col name doesn't have to be changed downstream
    dplyr::mutate(SOE.24 = Guild) |>
    dplyr::select(-Reviewed)

  if (save_clean) {
    usethis::use_data(species_groupings, overwrite = T)
  } else {
    return(species_groupings)
  }
}

get_species_groupings(save_clean = T)
