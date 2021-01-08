library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

# input files ----
phyto_csv <- "1998_2020-OCCCI-PHYSIZE-STATS-NES_EPU_NOESTUARIES-V2021-SOE_FORMAT.csv"

phyto_size <- read.csv(file.path(raw.dir, phyto_csv)) %>%
  dplyr::select(PERIOD, UNITS, VARIABLE, VALUE, SUBAREA) %>%
  dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                EPU =SUBAREA, Value = VALUE)

# metadata ----
attr(phyto_size, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/chl-pp.html"
attr(phyto_size, "data_files")   <- list(
  phyto_csv = phyto_csv
)
attr(phyto_size, "data_steward") <- c(
  "Kimberly Hyde <kimberly.hyde@noaa.gov>")

usethis::use_data(phyto_size, overwrite = T)
