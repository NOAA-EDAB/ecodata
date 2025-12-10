#Processing for NEAMAP inshore survey

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

raw.dir <- here::here("data-raw")
mab_inshore_survey_csv <- "NEAMAP_SOE_INDICES_2025.xlsx"
get_mab_inshore_survey <- function(save_clean = F){

  mab_inshore_survey <- readxl::read_excel(file.path(raw.dir, mab_inshore_survey_csv)) %>%
    tidyr::unite(Var, "Category","Season", sep = " ") %>%
    dplyr::rename(Time = Year,
                  Value = Index) %>%
    dplyr::mutate(Units = c("kg tow^-1"),
           EPU = "MAB")%>%
    tidyr::pivot_longer(cols = !c(Time, Var, Units, EPU),
                        names_to = "Var2", values_to = "Value" ) %>%
    dplyr::mutate(Var = paste0(Var, "-", Var2)) %>%
    dplyr::select(Time, Var, Value, EPU)

  if (save_clean){
    usethis::use_data(mab_inshore_survey, overwrite = T)
  } else {
    return(mab_inshore_survey)
  }
}
get_mab_inshore_survey(save_clean = T)
