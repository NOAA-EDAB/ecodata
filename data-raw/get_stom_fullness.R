library(dplyr)
library(tidyr)
library(stringr)


raw.dir <- here::here("data-raw")

get_stom_fullness <- function(save_clean = F){

  stom_fullness <- read.csv(file.path(raw.dir,"AnnualStomFullnessEPU_2019.csv")) %>%
    dplyr::select(year, EPU, Species, AvgStomFullEPU) %>%
    rename(Time = year,
           Var = Species) %>%
    distinct() %>%
    group_by(EPU, Var) %>%
    mutate(Value = AvgStomFullEPU-mean(AvgStomFullEPU)) %>%
    ungroup() %>%
    mutate(Units = c("anom")) %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  if (save_clean){
    usethis::use_data(stom_fullness, overwrite = T)
  } else {
    return(stom_fullness)
  }

}
get_stom_fullness(save_clean = T)
