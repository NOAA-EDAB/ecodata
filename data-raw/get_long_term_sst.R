#process ERSST long-term SST data

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")

get_long_term_sst <- function(save_clean = F){
  long_term_sst <- read.csv(file.path(raw.dir,"ersst annual mean.csv")) %>%
    dplyr::rename(Time = Year,
                  Value = Mean) %>%
    dplyr::mutate(Var = "long-term sst",
           EPU = "All",
           Units = "degreesC") %>%
    tibble::as_tibble()

  # metadata ----
  attr(long_term_sst, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/long-term-sea-surface-temperature.html"
  attr(long_term_sst, "data_steward") <- c(
    "Kevin Friedland <kevin.friedland@noaa.gov>")
  attr(long_term_sst, "plot_script") <- list(
    `ltl_MAB` = "LTL_MAB.Rmd-long-term-sst.R")

  if (save_clean) {
    usethis::use_data(long_term_sst, overwrite = T)
  } else {
    return(long_term_sst)
  }
}
get_long_term_sst(save_clean = T)
