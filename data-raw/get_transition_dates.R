library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
trans_dates_csv <- "transdates_101225.csv"

get_transition_dates <- function(save_clean = F){

  trans_dates <- read.csv(file.path(raw.dir, trans_dates_csv))

  if (save_clean){
    usethis::use_data(trans_dates, overwrite = T)
  } else {
    return(trans_dates)
  }
}
get_transition_dates(save_clean = T)
