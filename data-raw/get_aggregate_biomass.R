## Aggregate biomass

### Documentation https://noaa-edab.github.io/tech-doc/aggroups.html

library(dplyr)

raw.dir <- here::here("data-raw")
aggregate_biomass_Rds <- "aggregate_biomass.rds"

get_aggregate_biomass <- function(save_clean = F){

  aggregate_biomass <- readRDS(file.path(raw.dir, aggregate_biomass_Rds))

  if (save_clean){
    usethis::use_data(aggregate_biomass, overwrite = T)
  } else {
    return(aggregate_biomass)
  }
}
get_aggregate_biomass(save_clean = T)
