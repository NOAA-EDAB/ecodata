library(tidyverse)

raw.dir <- here::here("data-raw")
wind_occupancy_csv <- "wind-occupancy-1.csv"
get_wind_occupancy <- function(save_clean = F){

  wind_occupancy <- read.csv(file.path(raw.dir, wind_occupancy_csv )) %>%
    tibble::as_tibble()

  # metadata ----
  attr(wind_occupancy, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/wind-lease-areas-and-habitat-occupancy-overlap.html"
  attr(wind_occupancy, "data_files")   <- list(
    wind_occupancy_csv = wind_occupancy_csv)
  attr(wind_occupancy, "data_steward") <- c(
    "Kevin Friedland <kevin.freidland@noaa.gov>")
  attr(wind_occupancy, "plot_script") <- list(
    `hd_MAB` = "human_dimensions_MAB.Rmd-wind-occupancy.R")

  if (save_clean){
    usethis::use_data(wind_occupancy, overwrite = T)
  } else {
    return(wind_occupancy)
  }
}
get_wind_occupancy(save_clean = T)
