library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

raw.dir <- here::here("data-raw")

### Zooplankton Diversity
get_zoo_diversity <- function(save_clean = F){

  zoo_diversity <- read_excel(file.path(raw.dir,"NEFSCZooplankton_v3_6b_v2018.xlsx"), sheet = "Diversity") %>%
    dplyr::select(-Source) %>%
    dplyr::rename(Time = Year,
                  EPU = Region) %>%
    mutate(Value  = as.numeric(Value))

  if (save_clean){
    usethis::use_data(zoo_diversity, overwrite = T)
  } else {
    return(zoo_diversity)
  }
}
get_zoo_diversity(save_clean = T)



