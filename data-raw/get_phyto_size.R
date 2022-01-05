library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

# input files ----
phyto_csv <- "19980101_20211231-OCCCI-PHYSIZE-STATS_ANOMS-NES_EPU_NOESTUARIES-SOE_V2022-SOE_FORMAT.csv"

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
attr(phyto_size, "plot_script") <- list(
  `ltl_MAB_weekly` = "LTL_MAB.Rmd-weekly-phyto-size.R",
  `ltl_NE_weekly` = "LTL_NE.Rmd-weekly-phyto-size.R",
  `ltl_NE_weekly-gb` = "LTL_NE.Rmd-weekly-phyto-size-gb.R",
  `ltl_NE_weekly-gom` = "LTL_NE.Rmd-weekly-phyto-size-gom.R")

usethis::use_data(phyto_size, overwrite = T)
