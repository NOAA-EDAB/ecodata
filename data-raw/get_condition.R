## get condition
## condition factor calculations from Laurel Smith using
## BTS data by species aggregated to annual condition index.

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
condition_Rds <- "condition.rds"

get_condition <- function(save_clean = F) {
  condition <- readRDS(file.path(raw.dir, condition_Rds))

  if (save_clean) {
    usethis::use_data(condition, overwrite = T)
  } else {
    return(condition)
  }
}
get_condition(save_clean = T)
