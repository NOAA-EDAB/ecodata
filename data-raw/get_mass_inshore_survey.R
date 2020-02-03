#MA state survey data

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")

get_mass_survey <- function(save_clean){

  load(file.path(raw.dir,"Aggregate_Mass_Survey_biomass_20.Rdata"))

  mass_inshore_survey <- mass.survey %>%
    dplyr::mutate(EPU = c("GB"))

  if (save_clean){
    usethis::use_data(mass_inshore_survey, overwrite = T)
  } else {
    return(mass_inshore_survey)
  }
}
get_mass_survey(save_clean = T)
