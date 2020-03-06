library(dplyr)
library(tidyr)
library(stringr)


raw.dir <- here::here("data-raw")

get_stom_fullness <- function(save_clean = F){

  stom_fullness <- read.csv(file.path(raw.dir,"AnnualStomFullnessEPU_2019.csv")) %>%
    dplyr::select(year, EPU, Species, AvgStomFullEPU) %>%
    dplyr::rename(Time = year,
           Var = Species) %>%
    dplyr::distinct() %>%
    dplyr::group_by(EPU, Var) %>%
    dplyr::mutate(Value = AvgStomFullEPU-mean(AvgStomFullEPU)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Units = c("anom")) %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  if (save_clean){
    usethis::use_data(stom_fullness, overwrite = T)
  } else {
    return(stom_fullness)
  }

}
get_stom_fullness(save_clean = T)
