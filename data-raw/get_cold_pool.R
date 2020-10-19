### Cold pool index from Chris Melrose

library(dplyr)
library(tidyr)


raw.dir <- here::here("data-raw")

cold_pool_csv<- "cold_pool_index.csv"

get_cold_pool <- function(save_clean = F){

  cold_pool <- read.csv(file.path(raw.dir, cold_pool_csv)) %>%
    dplyr::rename(EPU = Region,
                  Time = Year,
                  Value = VAR)

  if(save_clean){
    usethis::use_data(cold_pool, overwrite = T)
  } else {
    return(cold_pool)
  }
  # metadata ----
  attr(cold_pool, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/cold-pool-index.html"
  attr(cold_pool, "data_files")   <- list(
    cold_pool_csv = cold_pool_csv)
  attr(cold_pool, "data_steward") <- c(
    "Chris Melrose <chris.melrose@noaa.gov>")

}
get_cold_pool(save_clean = T)
