### Cold pool index from Joe Caracappa
raw.dir <- here::here("data-raw")

cold_pool_csv <- "cold_pool_indices_1959_2026 - Joseph Caracappa - NOAA Federal.csv"

### Cold Pool time series
get_cold_pool <- function(save_clean = F) {
  cold_pool <- read.csv(file.path(raw.dir, cold_pool_csv)) |>
    dplyr::select(Time, EPU, Var, Value) |>
    tibble::as_tibble()

  if (save_clean) {
    usethis::use_data(cold_pool, overwrite = T)
  } else {
    return(cold_pool)
  }
}
get_cold_pool(save_clean = T)
