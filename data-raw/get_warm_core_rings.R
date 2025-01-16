# Warm core rings

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

raw.dir <- here::here("data-raw")
wcr_xlsx <- "WCR.xlsx"
get_warm_core_rings <- function(save_clean = F){
  wcr <- read_excel(file.path(raw.dir,wcr_xlsx)) %>%
    dplyr::select(Time, WCR) %>%
    dplyr::rename(Value = WCR) %>%
    tibble::add_row(Time = 2020, Value = 21) %>% #add 2020 data point
    tibble::add_row(Time = 2021, Value = 29) %>% #add 2021 data point
    tibble::add_row(Time = 2022, Value = 29) %>% #add 2022 data point
    tibble::add_row(Time = 2023, Value = 18) %>% #add 2023 data point
    tibble::add_row(Time = 2024, Value = 23) %>% #add 2024 data point
        dplyr::mutate(EPU = c("All"),
           Var = c("Warm Core Rings"),
           Units = c("n")) %>% tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  if (save_clean){
    usethis::use_data(wcr, overwrite = T)
  } else {
    return(wcr)
  }
  # metadata ----
  attr(wcr, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/warm-core-rings.html"
  attr(wcr, "data_files")   <- list(
    wcr_xlsx = wcr_xlsx)
  attr(wcr, "data_steward") <- c(
    "Avijit Gangopadhyay <avijit.gangopadhyay@umassd.edu>")
}
get_warm_core_rings(save_clean = T)
