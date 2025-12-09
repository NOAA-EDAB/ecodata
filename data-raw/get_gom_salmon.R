#### Gulf of Maine Atlantic salmon

library(tidyverse)
library(readr)
library(readxl)

raw.dir <- here::here("data-raw")

gom_salmon_csv<-"Gulf of Maine Atlantic Salmon 2025 Updated21NOV2025 - John Kocik - NOAA Federal.xlsx"
get_gom_salmon <- function(save_clean = F){

  gom_salmon<-read_excel(file.path(raw.dir,gom_salmon_csv), col_names = TRUE) %>%
    dplyr::rename("Year" = "Return Year",
                  "Total" = "GoM Salmon Total",
                  "PSAR" = "PSAR - 2SW") %>%
    tidyr::pivot_longer(c("Total", "PSAR"), names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Units = c("number of salmon"),
                  Value = as.numeric(Value))  %>%
    dplyr::arrange(Var) %>%
    dplyr::mutate(Units = c(rep("percent return rate",54),rep("number of salmon",54))) %>%
    dplyr::arrange(Year) %>%
    dplyr::rename(Time = Year) %>%
    dplyr::select(!"Figure ##. Return Rate proportions and abundance of Atlantic salmon.")

  if (save_clean){
    usethis::use_data(gom_salmon, overwrite = T)
  } else {
    return(gom_salmon)
  }
}
get_gom_salmon(save_clean = T)
