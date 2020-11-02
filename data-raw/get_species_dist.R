#Aggregate species distribution metrics for the Northeast Shelf

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
species_dist_csv <- "sp dist.csv"
get_species_dist <- function(save_clean = F){

  species_dist <- read.csv(file.path(raw.dir, species_dist_csv))  %>%
    dplyr::rename(depth = DEPTH,
                  Latitude = LAT,
                  Longitude = LON,
                  `along-shelf distance` = ASD,
                  `distance to coast` = DTC,
                  Time = Year) %>%
    tidyr::pivot_longer(-Time, names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(EPU = "All",
           Units = ifelse(stringr::str_detect(Var,"distance"),"km",
                          ifelse(stringr::str_detect(Var,"Latitude"),
                                     "degreesN",ifelse(stringr::str_detect(Var,"Longitude"),
                                                      "degreesW",ifelse(stringr::str_detect(Var, "depth"),
                                                                        "m",NA)))))
  if (save_clean){
    usethis::use_data(species_dist, overwrite = T)
  } else {
    return(species_dist)
  }
  # metadata ----
  attr(species_dist, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/species-distribution-indicators.html"
  attr(species_dist, "data_files")   <- list(
    species_dist_csv = species_dist_csv)
  attr(species_dist, "data_steward") <- c(
    "Kevin Friedland <kevin.freidland@noaa.gov>")
}
get_species_dist(save_clean = T)
