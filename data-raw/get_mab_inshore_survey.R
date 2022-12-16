#Processing for NEAMAP inshore survey

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

raw.dir <- here::here("data-raw")
mab_inshore_survey_csv <- "NEAMAP_SOE INDICES_2022.csv"
get_mab_inshore_survey <- function(save_clean = F){

  mab_inshore_survey <- read.csv(file.path(raw.dir,
                                             mab_inshore_survey_csv)) %>%
    tidyr::unite(Var, "Category","Season", sep = " ") %>%
    dplyr::rename(Time = Year,
                  Value = Index) %>%
    dplyr::mutate(Units = c("kg tow^-1"),
           EPU = "MAB")%>%
    tidyr::pivot_longer(cols = !c(Time, Var, Units, EPU),
                        names_to = "Var2", values_to = "Value" ) %>%
    dplyr::mutate(Var = paste0(Var, "-", Var2)) %>%
    dplyr::select(Time, Var, Value, EPU)

  # metadata ----
  attr(mab_inshore_survey, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/inshoresurvdat.html"
  attr(mab_inshore_survey, "data_files")   <- list(
    mab_inshore_survey_csv = mab_inshore_survey_csv)
  attr(mab_inshore_survey, "data_steward") <- c(
    "James Gartland <jgartlan@vim.edu>")
  attr(mab_inshore_survey, "plot_script") <- list(
    `mf_MAB` = "macrofauna_MAB.Rmd-mab-inshore-survey.R")

  if (save_clean){
    usethis::use_data(mab_inshore_survey, overwrite = T)
  } else {
    return(mab_inshore_survey)
  }
}
get_mab_inshore_survey(save_clean = T)
