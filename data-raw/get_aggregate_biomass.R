## Aggregate biomass

### Documentation https://noaa-edab.github.io/tech-doc/aggroups.html

library(dplyr)

raw.dir <- here::here("data-raw")

aggregate_biomass_RData <- "Aggregate_Survey_biomass_22.RData"
aggregate_biomass_shelf<- "Aggregate_Survey_biomass_shelfwide_22.RData"
get_aggregate_biomass <- function(save_clean = F){

  load(file.path(raw.dir, aggregate_biomass_RData))

  aggregate_biomass <- survey.data %>%
    dplyr::rename(EPU = Region) %>%
    tibble::as_tibble()%>%
    dplyr::select(Time, Var, Value, EPU, Units)

  load(file.path(raw.dir, aggregate_biomass_shelf))

  aggregate_shelf<- survey.data %>%
    dplyr::rename(EPU = Region) %>%
    tibble::as_tibble()%>%
    dplyr::select(Time, Var, Value, EPU, Units)

  aggregate_biomass<-aggregate_biomass %>% rbind(aggregate_shelf)

  # metadata ----
  attr(aggregate_biomass, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/aggroups.html"
  attr(aggregate_biomass, "data_files")   <- list(
    aggregate_biomass_RData = aggregate_biomass_RData)
  attr(aggregate_biomass, "data_steward") <- c(
    "Sean Lucey <sean.lucey@noaa.gov>")
  attr(aggregate_biomass, "plot_script") <- list(
    `mf_MAB` = "macrofauna_MAB.Rmd-aggregate-biomass.R",
    `mf_NE` = "macrofauna_NE.Rmd-aggregate-biomass.R")

  if (save_clean){
    usethis::use_data(aggregate_biomass, overwrite = T)
  } else {
    return(aggregate_biomass)
  }
}
get_aggregate_biomass(save_clean = T)

















