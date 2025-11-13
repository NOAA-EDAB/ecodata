library(dplyr)
library(stringr)
library(tidyr)

raw.dir <- here::here("data-raw")
surv_shan_Rds <- "survey_shannon.rds"

get_surv_shan <- function(save_clean = F){

  survey_shannon <- readRDS(file.path(raw.dir, surv_shan_Rds))

  if (save_clean){
    usethis::use_data(survey_shannon, overwrite = T)
  } else {
    return(exp_n)
  }
}
get_surv_shan(save_clean = T)
