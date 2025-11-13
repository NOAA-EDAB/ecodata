# Processing single-species stock mortality and biomass data

# Read more about this data at https://noaa-edab.github.io/tech-doc/stockstatus.html

library(dplyr)
library(tidyr)
library(ggplot2)

raw.dir <- here::here('data-raw')
stock_status_Rds <- "stock_status.rds"

get_stocks <- function(save_clean = F){

  stock_status <- readRDS(file.path(raw.dir, stock_status_Rds))

  if (save_clean){
    usethis::use_data(stock_status, overwrite = T)
  } else {
    return(stock_status)
  }
}

get_stocks(save_clean = T)
