## Aggregate biomass

### Documentation https://noaa-edab.github.io/tech-doc/aggroups.html

library(dplyr)

raw.dir <- here::here("data-raw")

get_aggregate_biomass <- function(save_clean = F){

  load(file.path(raw.dir, "Aggregate_Survey_biomass_20 (5).RData"))

  aggregate_biomass <- survey %>%
    dplyr::rename(EPU = Region)


  if (save_clean){
    usethis::use_data(aggregate_biomass, overwrite = T)
  } else {
    return(aggregate_biomass)
  }

}
get_aggregate_biomass(save_clean = T)
