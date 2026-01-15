library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

# input files ----
chl_ppd_ann_csv <- "MERGED_ANNUAL_SUM-NES_EPU_NOESTUARIES-PPD-VGPM2_CHLOR_A-CCI-STATS-V2026-SOE_FORMAT.csv"

# transformation ----
annual_chl_pp <- read.csv(file.path(raw.dir, chl_ppd_ann_csv)) %>%
  dplyr::select(PERIOD, UNITS, VARIABLE, VALUE, SUBAREA) %>%
  dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                EPU =SUBAREA, Value = VALUE) %>%
  tibble::as_tibble()

usethis::use_data(annual_chl_pp, overwrite = T)
