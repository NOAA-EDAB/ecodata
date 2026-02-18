library(dplyr)
library(stringr)
library(tidyr)

raw.dir <- here::here("data-raw")
exp_n_Rds <- "exp_n.rds"

get_exp_n <- function(save_clean = F) {
  exp_n <- readRDS(file.path(raw.dir, exp_n_Rds))

  if (save_clean) {
    usethis::use_data(exp_n, overwrite = T)
  } else {
    return(exp_n)
  }
}
get_exp_n(save_clean = T)
