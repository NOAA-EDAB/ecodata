#### Chesapeake bay salinity

library(tidyverse)
library(readr)

raw.dir <- here::here("data-raw")

ch_bay_sal_csv<-"GR_Salinity_Data.csv"
get_ch_bay_sal <- function(save_clean = F){

  ch_bay_sal<-read_csv(file.path(raw.dir,ch_bay_sal_csv), col_names = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::rename("Year" = "2021 Daily",
                  "YearLTA" = "2010-2020 avg",
                  "minLTA" = "2010-2020 min",
                  "maxLTA" = "2010-2020 max") %>%
    tidyr::pivot_longer(c("Year", "YearLTA", "minLTA", "maxLTA"), names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Units = c("C"),
                  Value = as.numeric(Value))  %>%
    dplyr::rename(Time = Date) %>%
    tidyr::separate(Time, c("Time", "Trash"), sep = " ") %>%
    dplyr::select(-Trash) %>%
    dplyr::mutate(Time = lubridate::mdy(Time),
                  EPU = c("MAB"))%>%
    dplyr::select(Time, Var, Value, EPU, Units)

  # metadata ----
  attr(ch_bay_sal, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/chesapeake-bay-salinity-and-temperature.html"
  attr(ch_bay_sal, "data_files")   <- list(
    ch_bay_sal_csv = ch_bay_sal_csv)
  attr(ch_bay_sal, "data_steward") <- c(
    "Charles Pellerin <charles.pellerin@noaa.gov>",
    "Bruce Vogt <bruce.vogt@noaa.gov")
  attr(ch_bay_sal, "plot_script") <- list(
    `ltl_MAB` = "LTL_MAB.Rmd-ch-bay-sal.R")

  if (save_clean){
    usethis::use_data(ch_bay_sal, overwrite = T)
  } else {
    return(ch_bay_sal)
  }
}
get_ch_bay_sal(save_clean = T)
