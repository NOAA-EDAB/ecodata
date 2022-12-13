## Habitat Diversity

library(dplyr)
library(tidyr)
library(lubridate)

raw.dir <- here::here("data-raw")
hab_rich_csv<-"NRHA_richness_by_EPU - Chris Haak.csv"
hab_shannon_csv<- "shannon - Laurel Smith - NOAA Federal.csv"

get_habitat_diversity <- function(save_clean = F){

  richness <- read.csv(file.path(here::here(raw.dir, hab_rich_csv))) %>%
    dplyr::select(!c(YEAR, EPU)) %>%
    tidyr::separate(X, into = c("EPU", "Time"), sep = "_") %>%
    tidyr::pivot_longer(cols = c(mean,median,lower,upper),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::select(Time, Var, Value, EPU) %>%
    dplyr::mutate(Var = paste0(Var, "-Richness"))

  shannon <- read.csv(file.path(here::here(raw.dir, hab_shannon_csv))) %>%
    tidyr::separate(EPU_Year, into = c("EPU", "Time"), sep = "_") %>%
    dplyr::mutate(Var = c("Shannon")) %>%
    dplyr::rename(Shannon, "Value" = "Shannon")

  habitat_diversity<- rbind(richness, shannon)
  if (save_clean){
    usethis::use_data(habitat_diversity, overwrite = T)
  } else {
    return(habitat_diversity)
  }
}
get_habitat_diversity(save_clean = T)
