#Ichthyoplankton diversity

library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

raw.dir <- here::here("data-raw")


get_ichthyo_diversity <- function(save_clean = F){

  ichthyo_diversity <- read_excel(file.path(raw.dir,"NEFSCIchthyoplankton_v3_6.xlsx")) %>%
    dplyr::select(-Source) %>%
    dplyr::rename(Time = Year,
                  EPU = Region) %>%
    dplyr::mutate(Value  = as.numeric(Value))

  if (save_clean){
    usethis::use_data(ichthyo_diversity, overwrite = T)
  } else {
    return(ichthyo_diversity)
  }
}
get_ichthyo_diversity(save_clean = T)





