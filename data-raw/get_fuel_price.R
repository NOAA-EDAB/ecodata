# Process fuel price

# Monthly time series of diesel fuel prices based on Legacy No. 2 Diesel (1986-2006) and Ultra-Low Sulfur Diesel (2006-present)
# Data are calculated in the SOE_Workflows repo

raw.dir <- here::here("data-raw")
fuel_prices_input <- "fuel_prices.rds"

get_fuel_prices <- function(save_clean = F) {
  fuel_prices <- readRDS(file.path(raw.dir, fuel_prices_input))

  if (save_clean) {
    usethis::use_data(fuel_prices, overwrite = T)
  } else {
    return(fuel_prices)
  }
}
get_fuel_prices(save_clean = T)
