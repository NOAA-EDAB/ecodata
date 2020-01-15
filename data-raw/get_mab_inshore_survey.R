#Processing for NEAMAP inshore survey

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

raw.dir <- here::here("data-raw")

get_mab_inshore_survey <- function(save_clean = F){
  mab_inshore_survey <- read_excel(file.path(raw.dir,
                                             "NEAMAP_SOE INDICES_2019_final.xlsx")) %>%
     gather(.,Var,Value,-Year,-Season,-Category) %>%
     unite(., Var, c("Season","Category","Var"), sep = " ") %>%
     dplyr::rename(Time = Year) %>%
     mutate(Var = tolower(Var),
            Units = ifelse(str_detect(Var,"index"),
                           "kg tow^-1","CV"),
            EPU = "MAB")

  if (save_clean){
    usethis::use_data(mab_inshore_survey, overwrite = T)
  } else {
    return(mab_inshore_survey)
  }
}
get_mab_inshore_survey(save_clean = T)
