# Wind Revenue

raw.dir <- here::here("data-raw")
wind_revenue_raw <- "ecodata_wind_revenue_2026-10-28 - Abigail Tyrell - NOAA Federal.csv"

get_wind_revenue <- function(save_clean = F) {
  wind_revenue <- read.csv(file.path(raw.dir, wind_revenue_raw)) |>
    tibble::as_tibble()

  if (save_clean) {
    usethis::use_data(wind_revenue, overwrite = TRUE)
  } else {
    return(wind_revenue)
  }
}
get_wind_revenue(save_clean = T)
