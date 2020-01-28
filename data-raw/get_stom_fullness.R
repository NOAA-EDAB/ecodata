library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

raw.dir <- here::here("data-raw")

get_stom_fullness <- function(save_clean = F){
  stom_fullness <- read_excel(file.path(raw.dir,"AnnualStomFullnessEPU_2019.xlsx"),
                                   sheet = 2) %>%
    group_by(YEAR, Species) %>%
    filter(AvgStomFullEPU > 0) %>%
    mutate(Value = AvgStomFullEPU-mean(AvgStomFullEPU)) %>%
    ungroup() %>%
    filter(YEAR >2001) %>%
    rename(Time = YEAR,
           Var = Species) %>%
    mutate(Units = c("anom")) %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  if (save_clean){
    usethis::use_data(stom_fullness, overwrite = T)
  } else {
    return(stom_fullness)
  }

}
get_stom_fullness(save_clean = T)
