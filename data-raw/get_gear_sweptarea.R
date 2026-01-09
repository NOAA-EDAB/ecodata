library(dplyr)
library(stringr)
library(tidyr)

raw.dir <- here::here("data-raw")
gear_sweptarea_file <- "effective swept area - Andrew Applegate - NOAA Affiliate.RData"

get_gear_sweptarea <- function(save_clean = F){

  temp.env <- new.env()
  load(file.path(raw.dir,gear_sweptarea_file))

  exp_n <- readRDS(file.path(raw.dir, exp_n_Rds))

  if (save_clean){
    usethis::use_data(exp_n, overwrite = T)
  } else {
    return(exp_n)
  }
}
get_exp_n(save_clean = T)
