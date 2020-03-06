#Processing for NEAMAP inshore survey

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

raw.dir <- here::here("data-raw")

get_mab_inshore_survey <- function(save_clean = F){

  mab_inshore_survey <- read_excel(file.path(raw.dir,
                                             "NEAMAP_SOE INDICES_2019_final.xlsx")) %>%
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
}
get_mab_inshore_survey(save_clean = T)
