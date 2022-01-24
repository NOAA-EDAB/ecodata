library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

# input files ----
ppd_csv <- "19980101_20211231-OCCCI_MODISA-PPD-STATS_ANOMS-NES_EPU_NOESTUARIES-SOE_V2022-SOE_FORMAT.csv"
chl_csv <- "19980101_20211231-OCCCI_GLOBCOLOUR-CHLOR_A-STATS_ANOMS-NES_EPU_NOESTUARIES-SOE_V2022-SOE_FORMAT.csv"

# transformation ----
ppd <- read.csv(file.path(raw.dir, ppd_csv)) %>%
  #dplyr::mutate(ALGORITHM = word(str_replace(ALGORITHM, "_", " "))) %>%
  #tidyr::unite(.,VARIABLE, c("VARIABLE","SENSOR","ALGORITHM"), sep = " ") %>%
  # dplyr::mutate(VARIABLE = ifelse(stringr::str_detect(FILENAME, "1998_2020"),
  #                           paste(VARIABLE,"1998_20120")))
  #                          # ifelse(stringr::str_detect(FILENAME, "1998_2020"),
  #                          #        paste(VARIABLE, "1998_2018"),
  #                          #        ifelse(stringr::str_detect(FILENAME, "1997_2019"),
  #                          #               paste(VARIABLE, "1997_2019"),
  #                          #               ifelse(stringr::str_detect(FILENAME, "1997_2018"),
  #                          #                      paste(VARIABLE, "1997_2018"),
  #                          #                      VARIABLE))))) %>%
  dplyr::select(PERIOD, UNITS, VARIABLE, VALUE, SUBAREA) %>%
  dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                EPU =SUBAREA, Value = VALUE)

chl <- read.csv(file.path(raw.dir, chl_csv)) %>%
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
  dplyr::select(PERIOD, UNITS, VARIABLE, VALUE, SUBAREA) %>%
  dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                EPU = SUBAREA, Value = VALUE)

chl_pp <- rbind(ppd,chl)

# metadata ----
attr(chl_pp, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/chl-pp.html"
attr(chl_pp, "data_files")   <- list(
  chl_csv = chl_csv,
  ppd_csv = ppd_csv)
attr(chl_pp, "data_steward") <- c(
  "Kimberly Hyde <kimberly.hyde@noaa.gov>")
attr(chl_pp, "plot_script") <- list(
  `ltl_NE_anom` = "LTL_NE.Rmd-chl-pp-anom.R")

usethis::use_data(chl_pp, overwrite = T)

