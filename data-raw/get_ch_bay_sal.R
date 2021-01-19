#### Chesapeake bay salinity

library(tidyverse)
library(readr)

raw.dir <- here::here("data-raw")

ch_bay_sal_csv<-"SR_Salinity.csv"
get_ch_bay_sal <- function(save_clean = F){

  ch_bay_sal<-read_csv(file.path(raw.dir,ch_bay_sal_csv), col_names = FALSE) %>%
    dplyr::mutate(Columns = c("UTCTime", "MinDataLim", "MaxDataLim", "AvgMinLim",
                              "AvgMaxLim","Daily19", "Daily18")) %>%
    tidyr::gather(Day, Value, -Columns) %>%
    dplyr::rename(Var = Columns,
                  Time = Day) %>%
    dplyr::mutate(EPU = c("MAB"),
                  Units = c("degree C"))

  # metadata ----
  attr(ch_bay_sal, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/chesapeake-bay-salinity.html"
  attr(ch_bay_sal, "data_files")   <- list(
    ch_bay_sal_csv = ch_bay_sal_csv)
  attr(ch_bay_sal, "data_steward") <- c(
    "Charles Pellerin <charles.pellerin@noaa.gov>",
    "Bruce Vogt <bruce.vogt@noaa.gov")

  if (save_clean){
    usethis::use_data(ch_bay_sal, overwrite = T)
  } else {
    return(ch_bay_sal)
  }
}
get_ch_bay_sal(save_clean = T)
