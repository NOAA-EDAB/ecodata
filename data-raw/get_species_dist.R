#Aggregate species distribution metrics for the Northeast Shelf

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
species_dist_Rds <- "species_dist.rds"

get_species_dist <- function(save_clean = F){

  species_dist <- readRDS(file.path(raw.dir, species_dist_Rds))

  if (save_clean){
    usethis::use_data(species_dist, overwrite = T)
  } else {
    return(species_dist)
  }
}

get_species_dist(save_clean = T)
