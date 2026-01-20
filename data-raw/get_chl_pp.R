library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

# input files ----
chl_pp_csv <- "chl_pp_input.csv"

# transformation ----
chl_pp <- read.csv(file.path(raw.dir, chl_pp_csv)) %>%
  dplyr::select(PERIOD, VARIABLE, VALUE, SUBAREA, UNITS) %>%
  dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                EPU = SUBAREA, Value = VALUE) %>%
  dplyr::mutate(Value = as.numeric(Value)) %>%
  dplyr::filter(Var != "ANNUAL_PPD_RATIO_ANOMALY") %>%
  dplyr::distinct(Time, Var, EPU, .keep_all = T)

chl_pp <- as_tibble(chl_pp)

usethis::use_data(chl_pp, overwrite = T)
