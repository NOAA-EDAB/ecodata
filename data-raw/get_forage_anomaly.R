## Foraage anomalies

library(tidyverse)

raw.dir <- here::here("data-raw")
forage_anomaly_csv<-"NEUS_ForageAnomalies - Harvey Walsh - NOAA Federal.csv"
get_forage_anomaly <- function(save_clean = F){

  forage_anomaly <- read.csv(file.path(raw.dir, forage_anomaly_csv)) %>%
    dplyr::mutate(Forage_Upper = Forage_Mean+Forage_SE,
                  Forage_Lower = Forage_Mean-Forage_SE) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with("Forage"), names_to = "Var", values_to = "Value") %>%
    dplyr::rename(Time = ï..Year)%>%
    tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU)

  # metadata ----
  attr(forage_anomaly, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc.html"
  attr(forage_anomaly, "data_files")   <- list(
    forage_anomaly_csv = forage_anomaly_csv)
  attr(forage_anomaly, "data_steward") <- c(
    " Harvey Walsh <harvey.walsh@noaa.gov>")
  attr(forage_anomaly, "plot_script") <- list(
    `mf_MAB` = "macrofauna_MAB.Rmd-forage-anomaly.R",
    `mf_NE` = "macrofauna_NE.Rmd-forage-anomaly.R")

  if (save_clean){
    usethis::use_data(forage_anomaly, overwrite = T)
  } else {
    return(forage_anomaly)
  }
}
get_forage_anomaly(save_clean = T)
