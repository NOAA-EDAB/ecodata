#MA state survey data

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
mass_inshore_survey_Rds <- "mass_inshore_survey.rds"

get_mass_survey <- function(save_clean) {
  mass_inshore_survey <- readRDS(file.path(raw.dir, mass_inshore_survey_Rds))

  if (save_clean) {
    usethis::use_data(mass_inshore_survey, overwrite = T)
  } else {
    return(mass_inshore_survey)
  }
}

get_mass_survey(save_clean = T)
