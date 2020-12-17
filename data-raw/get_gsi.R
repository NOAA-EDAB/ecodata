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

  if (save_clean){
    usethis::use_data(gsi, overwrite = T)
  } else {
    return(gsi)
  }
  # metadata ----
  attr(gsi, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/gulf-stream-index.html"
  attr(gsi, "data_files")   <- list(
    gsi_csv = gsi_csv)
  attr(gsi, "data_steward") <- c(
    "Vincent Saba <vincent.saba@noaa.gov>")
}
get_gsi(save_clean = T)
