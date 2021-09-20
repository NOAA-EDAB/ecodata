## Wind develop areas

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

raw.dir <- here::here("data-raw")
observed_sharks_csv<- "Observed_Sharks_SOE2020 - Debra Duarte - NOAA Federal.csv"

get_observed_sharks <- function(save_clean = F){
  observed_sharks <- read.csv(file.path(raw.dir,observed_sharks_csv)) %>%
    dplyr::rename(Time = YEAR,
                  Var = SHKGROUP,
                  Value = NSHARK_HAUL)

  # metadata ----
  attr(observed_sharks, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc.html"
  attr(observed_sharks, "data_files")   <- list(
    observed_sharks_csv = observed_sharks_csv)
  attr(observed_sharks, "data_steward") <- c(
    "Debra Duarte  <debra.duarte@noaa.gov")
  attr(observed_sharks, "plot_script") <- list(
    `mf_MAB` = "macrofauna_MAB.Rmd-observed-sharks.R",
    `mf_NE` = "macrofauna_NE.Rmd-observed-sharks.R")

  if (save_clean){
    usethis::use_data(observed_sharks, overwrite = T)
  } else {
    return(observed_sharks)
  }
}
get_observed_sharks(save_clean = T)
