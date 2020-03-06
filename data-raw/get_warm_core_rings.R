# Warm core rings

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

raw.dir <- here::here("data-raw")

get_warm_core_rings <- function(save_clean = F){
  wcr <- read_excel(file.path(raw.dir,"WCR.xlsx")) %>%
    dplyr::select(Time, WCR) %>%
    dplyr::rename(Value = WCR) %>%
    dplyr::mutate(EPU = c("All"),
           Var = c("Warm Core Rings"),
           Units = c("n"))

  if (save_clean){
    usethis::use_data(wcr, overwrite = T)
  } else {
    return(wcr)
  }

}
get_warm_core_rings(save_clean = T)
