# Processing for Gulf Stream Index data

# GSI = degrees latitude above the average Gulf Stream position based
# on ocean temperature at 200m (15 C) depth between 55W to 75W.

library(dplyr)
library(tidyr)
library(lubridate)

raw.dir <- here::here("data-raw")
gsi_csv<-"GSI_1993_2019.csv"
get_gsi <- function(save_clean = F){

  gsi <- read.csv(file.path(raw.dir, gsi_csv)) %>%
    dplyr::rename(Time = Month, Value = GSI) %>%
    dplyr::mutate(Var = "gulf stream index",
           Units = "latitude anomaly",
           EPU = "All")

  # metadata ----
  attr(gsi, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/gulf-stream-index.html"
  attr(gsi, "data_files")   <- list(
    gsi_csv = gsi_csv)
  attr(gsi, "data_steward") <- c(
    "Vincent Saba <vincent.saba@noaa.gov>")
  attr(gsi, "plot_script") <- list(
    `ltl_MAB` = "LTL_MAB.Rmd-gsi.R")

  if (save_clean){
    usethis::use_data(gsi, overwrite = T)
  } else {
    return(gsi)
  }
}
get_gsi(save_clean = T)


raw.dir <- here::here("data-raw")
gsi_csv<-"GSI.csv"
get_gsi_old <- function(save_clean = F){

  gsi_old <- read.csv(file.path(raw.dir, gsi_csv)) %>%
    dplyr::rename(Time = Month, Value = GSI) %>%
    dplyr::mutate(Var = "gulf stream index",
                  Units = "latitude anomaly",
                  EPU = "All")

  if (save_clean){
    usethis::use_data(gsi_old, overwrite = T)
  } else {
    return(gsi_old)
  }
  # metadata ----
  # attr(gsi_old, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/gulf-stream-index.html"
  # attr(gsi_old, "data_files")   <- list(
  #   gsi_csv = gsi_csv)
  # attr(gsi_old, "data_steward") <- c(
  #   "Vincent Saba <vincent.saba@noaa.gov>")

}
get_gsi_old(save_clean = T)
