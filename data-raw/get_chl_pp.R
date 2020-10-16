library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

# input files ----
ppd_csv <- "SOE_V2020_2-NES_EPU_NOESTUARIES-PPD-VGPM2-STATS_ANOMS-SEAWIFS_MODIS.csv"
chl_csv <- "SOE_V2020_2-NES_EPU_NOESTUARIES-CHLOR_A-PAN-STATS_ANOMS-SEAWIFS_MODIS-V2.csv"

# transformation ----
ppd <- read.csv(file.path(raw.dir, ppd_csv)) %>%
  dplyr::mutate(ALGORITHM = word(str_replace(ALGORITHM, "_", " "))) %>%
  tidyr::unite(.,VARIABLE, c("VARIABLE","SENSOR","ALGORITHM"), sep = " ") %>%
  dplyr::mutate(VARIABLE = ifelse(stringr::str_detect(FILENAME, "1998_2019"),
                           paste(VARIABLE,"1998_2019"),
                           ifelse(stringr::str_detect(FILENAME, "1998_2018"),
                                  paste(VARIABLE, "1998_2018"),
                                  ifelse(stringr::str_detect(FILENAME, "1997_2019"),
                                         paste(VARIABLE, "1997_2019"),
                                         ifelse(stringr::str_detect(FILENAME, "1997_2018"),
                                                paste(VARIABLE, "1997_2018"),
                                                VARIABLE))))) %>%
  dplyr::select(TIME, UNITS, VARIABLE, VALUE, REGION) %>%
  dplyr::rename(Time = TIME, Units = UNITS, Var = VARIABLE,
                EPU = REGION, Value = VALUE)

chl <- read.csv(file.path(raw.dir, chl_csv)) %>%
  dplyr::mutate(ALGORITHM = word(stringr::str_replace(ALGORITHM, "_", " "))) %>%
  tidyr::unite(.,VARIABLE, c("VARIABLE","SENSOR","ALGORITHM"), sep = " ") %>%
  dplyr::mutate(VARIABLE = ifelse(stringr::str_detect(FILENAME, "1998_2019"),
                           paste(VARIABLE,"1998_2019"),
                           ifelse(stringr::str_detect(FILENAME, "1998_2017"),
                                  paste(VARIABLE, "1998_2017"),
                                  ifelse(stringr::str_detect(FILENAME, "1997_2019"),
                                         paste(VARIABLE, "1997_2019"),
                                         ifelse(stringr::str_detect(FILENAME, "1997_2017"),
                                                paste(VARIABLE, "1997_2017"),
                                                VARIABLE))))) %>%
  dplyr::select(TIME, UNITS, VARIABLE, VALUE, REGION) %>%
  dplyr::rename(Time = TIME, Units = UNITS, Var = VARIABLE,
                EPU = REGION, Value = VALUE)

chl_pp <- rbind(ppd,chl)

# metadata ----
attr(chl_pp, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/chl-pp.html"
attr(chl_pp, "data_files")   <- list(
  chl_csv = chl_csv,
  ppd_csv = ppd_csv)
attr(chl_pp, "data_steward") <- c(
  "Kimberly Hyde <kimberly.hyde@noaa.gov>")

usethis::use_data(chl_pp, overwrite = T)

