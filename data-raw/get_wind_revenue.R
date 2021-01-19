library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
wind_rev_csv<- "Wind_Energy_Revenue - Geret DePiper - NOAA Federal.csv"

get_wind_revenue<- function(save_clean = F){
  wind_revenue <- read.csv(file.path(raw.dir,wind_rev_csv)) %>%
    dplyr::select(!X)

  if (save_clean){
    usethis::use_data(wind_revenue, overwrite = TRUE)
  } else {
    return(wind_revenue)
  }
  # metadata ----
  attr(wind_revenue, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc.html"
  attr(wind_revenue, "data_files")   <- list(
    wind_rev_csv = wind_rev_csv)
  attr(wind_revenue, "data_steward") <- c(
    "Geret DePiper <geret.depiper@noaa.gov>")
}
get_wind_revenue(save_clean = T)
