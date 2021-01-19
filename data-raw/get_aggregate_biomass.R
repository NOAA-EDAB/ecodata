## Aggregate biomass

### Documentation https://noaa-edab.github.io/tech-doc/aggroups.html

library(dplyr)

raw.dir <- here::here("data-raw")

aggregate_biomass_RData <- "Aggregate_Survey_biomass_20 (5).RData"
get_aggregate_biomass <- function(save_clean = F){

  load(file.path(raw.dir, aggregate_biomass_RData))

  aggregate_biomass <- survey %>%
    dplyr::rename(EPU = Region)

  # metadata ----
  attr(aggregate_biomass, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/aggroups.html"
  attr(aggregate_biomass, "data_files")   <- list(
    aggregate_biomass_RData = aggregate_biomass_RData)
  attr(aggregate_biomass, "data_steward") <- c(
    "Sean Lucey <sean.lucey@noaa.gov>")

  if (save_clean){
    usethis::use_data(aggregate_biomass, overwrite = T)
  } else {
    return(aggregate_biomass)
  }
}
get_aggregate_biomass(save_clean = T)
