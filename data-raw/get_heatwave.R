# Define file paths
raw.dir <- here::here("data-raw")
heatwave_input <- "heatwave.rds"

get_heatwave <- function(save_clean = F) {
  heatwave <- readRDS(file.path(raw.dir, heatwave_input))

  if (save_clean) {
    usethis::use_data(heatwave, overwrite = T)
  } else {
    return(heatwave)
  }
}

get_heatwave(save_clean = T)

#### get_heatwave_year get single year of heatwave

# Define file paths
raw.dir <- here::here("data-raw")
heatwave_year_input <- "heatwave_year.rds"

get_heatwave_year <- function(save_clean = F) {
  heatwave_year <- readRDS(file.path(raw.dir, heatwave_year_input))

  if (save_clean) {
    usethis::use_data(heatwave_year, overwrite = T)
  } else {
    return(heatwave_year)
  }
}
get_heatwave_year(save_clean = T)
