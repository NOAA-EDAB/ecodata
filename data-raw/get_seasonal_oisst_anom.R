#Analysis of OISST V2 data to extract seasonal SST time series
## Data gathered here
# "https://www.esrl.noaa.gov/psd/cgi-bin/DataAccess.pl?DB_dataset=
# NOAA+High-resolution+Blended+Analysis&DB_variable=Sea+Surface+
# Temperature&DB_statistic=Mean&DB_tid=81047&DB_did=132&DB_vid=2423"
# Spatial paraters
#lat:Begin (36N), End: (46N)
#lon:Begin (-77W), End: (-65W)

library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(ncdf4)
library(reshape2)
library(ecodata)
library(stringr)

### SST from ecopull
# https://github.com/kimberly-bastille/ecopull/blob/main/data/new_sst_anomaly.rda
raw.dir <- here::here("data-raw")
sst_rda <- "new_sst_anomaly.rda"

get_seasonal_oisst_anom <- function(save_clean = F) {
  load(file.path(raw.dir, sst_rda))
  seasonal_oisst_anom <- new_sst_anomaly %>%
    dplyr::rename(Time = Year) %>%
    tidyr::separate(Var, into = c("X", "Var", "A", "b"), sep = "_") %>%
    dplyr::select(Time, Var, Value, EPU) %>%
    dplyr::mutate(
      Var = dplyr::recode(
        Var,
        "winter" = "Winter",
        "spring" = "Spring",
        "summer" = "Summer",
        "fall" = "Fall"
      )
    )

  if (save_clean) {
    usethis::use_data(seasonal_oisst_anom, overwrite = T)
  } else {
    return(seasonal_oisst_anom)
  }
}
get_seasonal_oisst_anom(save_clean = T)
