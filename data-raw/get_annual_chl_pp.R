library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

# input files ----
chl_ppd_ann_csv <- "MERGED_ANNUAL_SUM-NES_EPU_NOESTUARIES-PPD-VGPM2_CHLOR_A-CCI-STATS-V2024-SOE_FORMAT.csv"

# transformation ----
annual_chl_pp <- read.csv(file.path(raw.dir, chl_ppd_ann_csv)) %>%
  dplyr::select(PERIOD, UNITS, VARIABLE, VALUE, SUBAREA) %>%
  dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                EPU =SUBAREA, Value = VALUE) %>%
  tibble::as_tibble()

# metadata ----
#attr(chl_pp, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/chl-pp.html"
#attr(chl_pp, "data_files")   <- list(
  #chl_csv = chl_csv,
  #ppd_csv = ppd_csv)
#attr(chl_pp, "data_steward") <- c(
  #"Kimberly Hyde <kimberly.hyde@noaa.gov>")
#attr(chl_pp, "plot_script") <- list(
  #`ltl_NE_anom` = "LTL_NE.Rmd-chl-pp-anom.R")

usethis::use_data(annual_chl_pp, overwrite = T)
