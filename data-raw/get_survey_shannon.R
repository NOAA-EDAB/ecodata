
library(dplyr)
library(stringr)
library(tidyr)

raw.dir <- here::here("data-raw")
surv_shan_rdata <- "survey_shannon.RData"
get_surv_shan <- function(save_clean = F){

  load(file.path(raw.dir, surv_shan_rdata))

  survey_shannon<-shannon.mean

  # metadata ----
  attr(survey_shannon, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc"
  attr(survey_shannon, "data_files")   <- list(
    surv_shan_rdata = surv_shan_rdata)
  attr(survey_shannon, "data_steward") <- c(
    "Sean Lucey <sean.lucey@noaa.gov>")
  attr(survey_shannon, "plot_script") <- list(
    `mf_MAB` = "macrofauna_MAB.Rmd-survey-shannon.R",
    `mf_NE` = "macrofauna_NE.Rmd-survey-shannon.R")

  if (save_clean){
    usethis::use_data(survey_shannon, overwrite = T)
  } else {
    return(exp_n)
  }
}
get_surv_shan(save_clean = T)
