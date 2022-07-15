library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

raw.dir <- here::here("data-raw")
zoo_diversity_xlsx <- "NEFSCZooplankton_v3_7_v2019.xlsx"
### Zooplankton Diversity
get_zoo_diversity <- function(save_clean = F){

  zoo_diversity <- read_excel(file.path(raw.dir,zoo_diversity_xlsx), sheet = "Diversity") %>%
    dplyr::select(-Source) %>%
    dplyr::rename(Time = Year,
                  EPU = Region) %>%
    mutate(Value  = as.numeric(Value)) %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  if (save_clean){
    usethis::use_data(zoo_diversity, overwrite = T)
  } else {
    return(zoo_diversity)
  }
  # metadata ----
  attr(zoo_diversity, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/zooabund.html"
  attr(zoo_diversity, "data_files")   <- list(
    zoo_diversity_xlsx = zoo_diversity_xlsx)
  attr(zoo_diversity, "data_steward") <- c(
    "Harvey Walsh <harvey.walsh@noaa.gov>",
    "Mike Jones <michael.jones@noaa.gov>",
    "Ryan Morse <ryan.morse@noaa.gov>")
}
get_zoo_diversity(save_clean = T)



