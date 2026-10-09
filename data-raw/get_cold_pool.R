### Cold pool index from Zhuomin Chen

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")

cold_pool_csv <- "cold_pool_indices_1959_2025.csv" # "Glorys12v1_ColdPool_Extents.nc" #File from Zhoumin
#cold_pool_mom6_csv <- "cold_pool_m6.csv - Laura Gruenburg - NOAA Federal.csv"

### Cold pool index from Hubert du Pontavice

## --------------------------------------------------------------------------------- ##
### Cold Pool time series
get_cold_pool <- function(save_clean = F) {
  #cold_pool_mom6 <- read.csv(file.path(raw.dir, cold_pool_mom6_csv)) %>%
  #dplyr::select(-c("X", "Unit")) %>%
  #dplyr::mutate(Var = "cold_pool_index", EPU = "MAB")

  cold_pool <- read.csv(file.path(raw.dir, cold_pool_csv)) %>%
    tidyr::pivot_longer(
      cols = c(
        "cold_pool_index",
        "se_cold_pool_index",
        "persistence_index",
        "se_persistence_index",
        "extent_index",
        "se_extent_index"
      ),
      names_to = "Var",
      values_to = "Value"
    ) %>%
    dplyr::mutate(EPU = c("MAB")) %>%
    dplyr::rename(Time = year)

  #cold_pool <- rbind(cold_pool, cold_pool_mom6)

  if (save_clean) {
    usethis::use_data(cold_pool, overwrite = T)
  } else {
    return(cold_pool)
  }
}
get_cold_pool(save_clean = T)
