#Ichthyoplankton diversity

library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

raw.dir <- here::here("data-raw")

ichthyo_diversity_xlsx <- "NEFSCIchthyoplankton_v3_7_v2019.xlsx"

get_ichthyo_diversity <- function(save_clean = F){

  ichthyo_diversity <- read_excel(file.path(raw.dir,ichthyo_diversity_xlsx), sheet =
                                    "Diversity") %>%
    dplyr::select(-Source) %>%
    dplyr::rename(Time = Year,
                  EPU = Region) %>%
    dplyr::mutate(Value  = as.numeric(Value))%>%
    dplyr::select(Time, Var, Value, EPU, Units)

  if (save_clean){
    usethis::use_data(ichthyo_diversity, overwrite = T)
  } else {
    return(ichthyo_diversity)
  }

  # metadata ----
  attr(ichthyo_diversity, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/plankton-diversity.html"
  attr(ichthyo_diversity, "data_files")   <- list(
    ichthyo_diversity_xlsx = ichthyo_diversity_xlsx)
  attr(ichthyo_diversity, "data_steward") <- c(
    "Harvey Walsh <harvey.walsh@noaa.gov>")
}
get_ichthyo_diversity(save_clean = T)





