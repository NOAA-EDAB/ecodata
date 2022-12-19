#MA state survey data

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
mass_inshore_survey_rdata <- "Aggregate_Mass_Survey_biomass_23.Rdata"
get_mass_survey <- function(save_clean){

  load(file.path(raw.dir,mass_inshore_survey_rdata))

  mass_inshore_survey <- mass.survey %>%
    dplyr::mutate(EPU = c("GB")) %>%
    tibble::as_tibble()%>%
    dplyr::select(Time, Var, Value, EPU, Units)

  if (save_clean){
    usethis::use_data(mass_inshore_survey, overwrite = T)
  } else {
    return(mass_inshore_survey)
  }
  # metadata ----
  attr(mass_inshore_survey, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/inshoresurvdat.html"
  attr(mass_inshore_survey, "data_files")   <- list(
    mass_inshore_survey_rdata = mass_inshore_survey_rdata)
  attr(mass_inshore_survey, "data_steward") <- c(
    "Sean Lucey <sean.lucey@noaa.gov>")
}
get_mass_survey(save_clean = T)
