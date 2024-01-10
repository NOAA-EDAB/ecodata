#### Gulf of Maine Atlantic salmon

library(tidyverse)
library(readr)

raw.dir <- here::here("data-raw")

gom_salmon_csv<-"Palka_Gulf of Maine Atlantic Salmon  - Debra Palka - NOAA Federal.xlsx"
get_gom_salmon <- function(save_clean = F){

  gom_salmon<-read_excel(file.path(raw.dir,gom_salmon_csv), col_names = TRUE) %>%
    dplyr::rename("Year" = "Return Year",
                  "Total" = "GoM Salmon Total",
                  "PSAR" = "PSAR - 2SW") %>%
    tidyr::pivot_longer(c("Total", "PSAR"), names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Units = c("number of salmon"),
                  Value = as.numeric(Value))  %>%
    dplyr::arrange(Var) %>%
    dplyr::mutate(Units = c(rep("percent return rate",51),rep("number of salmon",51))) %>%
    dplyr::arrange(Year) %>%
    dplyr::rename(Time = Year)

  # metadata ---- ### OLD METADATA FROM CH_BAY_SAL... NEEDS REPLACING
  #attr(ch_bay_sal, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/chesapeake-bay-salinity-and-temperature.html"
  #attr(ch_bay_sal, "data_files")   <- list(
    #ch_bay_sal_csv = ch_bay_sal_csv)
  #attr(ch_bay_sal, "data_steward") <- c(
    # "Charles Pellerin <charles.pellerin@noaa.gov>",
    # "Bruce Vogt <bruce.vogt@noaa.gov")
  #attr(ch_bay_sal, "plot_script") <- list(
     #`ltl_MAB` = "LTL_MAB.Rmd-ch-bay-sal.R")

  if (save_clean){
    usethis::use_data(gom_salmon, overwrite = T)
  } else {
    return(gom_salmon)
  }
}
get_gom_salmon(save_clean = T)
