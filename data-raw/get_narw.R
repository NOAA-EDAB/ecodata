
#Processing for North Atlantic Right Whale data

#See full documentation for these data at https://noaa-edab.github.io/tech-doc/right-whale-abundance.html

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

raw.dir <- here::here("data-raw")
narw_csv <- "Linden_NARW_abundance_2024-10-09 - Daniel Linden - NOAA Federal.csv"
calves_csv <- "Linden_NARW_Calves_1990-present - Daniel Linden - NOAA Federal.csv"

get_narw <- function(save_clean = F){

  calves <- read.csv(file.path(here::here(raw.dir,calves_csv))) %>%
    dplyr::rename(Time = Year) %>%
    dplyr::rename(Calves = Tot.Calves) %>%
    tidyr::pivot_longer(-Time, names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Units =  "n", EPU = "All")

  narw<- read.csv(file.path(here::here(raw.dir,narw_csv))) %>%
    dplyr::rename(Time = Year) %>%
    dplyr::select(!"X") %>%
    tidyr::pivot_longer(-Time, names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Units =  "n",
           EPU = "All") %>%
    dplyr::filter(!Value == "NA") %>%
    dplyr::mutate(Value == as.numeric(Value),
                  Time == as.numeric(Time))%>%
    dplyr::select(Time, Var, Value, EPU, Units)

  narw <- rbind(narw, calves) %>%
    dplyr::arrange(Var) %>%
    dplyr::arrange(Time)

  # metadata ----
  attr(narw, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/right-whale-abundance.html"
  attr(narw, "data_files")   <- list(
    narw_csv = narw_csv)
  attr(narw, "data_steward") <- c(
    "Chris Orphanides <chris.orphanides@noaa.gov>")
  attr(narw, "plot_script") <- list(
    `mf_MAB_abundance` = "macrofauna_MAB.Rmd-narw-abundance.R")

  if (save_clean){
    usethis::use_data(narw, overwrite = T)
  } else {
    return(narw)
  }
}
get_narw(save_clean = T)


