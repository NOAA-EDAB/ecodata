library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
trans_dates_rds <- "trans_dates.rds"

get_transition_dates <- function(save_clean = F) {
  trans_dates <- readRDS(file.path(raw.dir, trans_dates_rds))

  if (save_clean) {
    usethis::use_data(trans_dates, overwrite = T)
  } else {
    return(trans_dates)
  }
}
get_transition_dates(save_clean = T)
