#Processing for NEAMAP inshore survey

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

raw.dir <- here::here("data-raw")
mab_inshore_survey_xlsx <- "NEAMAP_SOE INDICES_2019_final.xlsx"
get_mab_inshore_survey <- function(save_clean = F){

  mab_inshore_survey <- read_excel(file.path(raw.dir,
                                             mab_inshore_survey_xlsx)) %>%
    tidyr::unite(Var, "Category","Season", sep = " ") %>%
    dplyr::rename(Time = Year,
                  Value = Index) %>%
    dplyr::mutate(Units = c("kg tow^-1"),
           EPU = "MAB")

  if (save_clean){
    usethis::use_data(mab_inshore_survey, overwrite = T)
  } else {
    return(mab_inshore_survey)
  }
  # metadata ----
  attr(mab_inshore_survey, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/inshoresurvdat.html"
  attr(mab_inshore_survey, "data_files")   <- list(
    mab_inshore_survey_xlsx = mab_inshore_survey_xlsx)
  attr(mab_inshore_survey, "data_steward") <- c(
    "James Gartland <jgartlan@vim.edu>")
}
get_mab_inshore_survey(save_clean = T)
