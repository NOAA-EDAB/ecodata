library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

# input files ----
chl_pp_csv <- "19980101_20241231-OCCCI-CHLOR_A_PPD_PSC_FMICRO_PSC_FNANO_PSC_FPICO_PSC_MICRO_PSC_NANO_PSC_PICO_PPD-STATS_ANOMS-NES_EPU_NOESTUARIES-SOE_PHYTOPLANKTON-SOE_FORMAT.csv"
# transformation ----
chl_pp <- read.csv(file.path(raw.dir, chl_pp_csv)) %>%
  #dplyr::mutate(ALGORITHM = word(stringr::str_replace(ALGORITHM, "_", " "))) %>%
  #tidyr::unite(.,VARIABLE, c("VARIABLE","SENSOR","ALGORITHM"), sep = " ") %>%
  # dplyr::mutate(VARIABLE = ifelse(stringr::str_detect(FILENAME, "1998_2019"),
  #                          paste(VARIABLE,"1998_2019"),
  #                          ifelse(stringr::str_detect(FILENAME, "1998_2017"),
  #                                 paste(VARIABLE, "1998_2017"),
  #                                 ifelse(stringr::str_detect(FILENAME, "1997_2019"),
  #                                        paste(VARIABLE, "1997_2019"),
  #                                        ifelse(stringr::str_detect(FILENAME, "1997_2017"),
  #                                               paste(VARIABLE, "1997_2017"),
  #                                               VARIABLE))))) %>%
  dplyr::select(PERIOD, VARIABLE, VALUE, SUBAREA, UNITS) %>%
  dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                EPU = SUBAREA, Value = VALUE) %>%
  dplyr::mutate(Value = as.numeric(Value)) %>%
  dplyr::filter(Var != "ANNUAL_PPD_RATIO_ANOMALY") %>%
  dplyr::distinct()

chl_pp <- as_tibble(chl_pp)

# metadata ----
attr(chl_pp, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/chl-pp.html"
attr(chl_pp, "data_files")   <- list(
  chl_pp_csv = chl_pp_csv)
attr(chl_pp, "data_steward") <- c(
  "Kimberly Hyde <kimberly.hyde@noaa.gov>")
attr(chl_pp, "plot_script") <- list(
  `ltl_NE_anom` = "LTL_NE.Rmd-chl-pp-anom.R")

usethis::use_data(chl_pp, overwrite = T)
