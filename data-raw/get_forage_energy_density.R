library(tidyverse)
library(readxl)
### File delivered almost unusable to save time edits made in excel
## Orginal File "2021SOE_Forage_ED_summary_Table - Mark Wuenschel - NOAA Federal.csv"


raw.dir <- here::here("data-raw")
energy_density_csv<-"Forage_Fish_ED_SOE_long_10292024 - Mark Wuenschel - NOAA Federal.csv"
get_forage_energy_density <- function(save_clean = F){

  energy_density <-
    read.csv(file.path(raw.dir, energy_density_csv)) %>%
    dplyr::mutate(Species = dplyr::recode(Species, "Loligo" = "Loligo squid")) %>%
    dplyr::mutate(Species = dplyr::recode(Species, "Illex" = "Illex squid")) %>%
    dplyr::rename(
      Energy.Density_Mean = "Energy.Density",
      Energy.Density_SD = "StdDev.of.Energy.Density",
      Time = Year
    ) %>%
    dplyr::select(Species,
                  Time,
                  Season,
                  N,
                  Energy.Density_Mean,
                  Energy.Density_SD) %>%
    dplyr::mutate(
      Var = paste0(Species, "/", Season),
      Energy.Density_Mean = as.numeric(Energy.Density_Mean),
      Energy.Density_SD = as.numeric(Energy.Density_SD),
      N = as.numeric(N),
      Time = as.numeric(Time)) %>%
    dplyr::select(-Season,-Species) %>%
    #tidyr::pivot_longer(cols = !c(Time,Var),  names_to = "Var2", values_to = "Value" ) %>%
    tidyr::pivot_longer(!c(Time, Var),
                        names_to = "Var2", values_to = "Value") %>%
    dplyr::mutate(Var = paste0(Var, "/", Var2),
                  EPU = c("NA")) %>%
    dplyr::select(Time, Var, Value, EPU) %>%
    dplyr::filter(!Time == "NA")

  if (save_clean){
    usethis::use_data(energy_density, overwrite = T)
  } else {

    return(energy_density)
  }
  # metadata ----
  attr(energy_density, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/forage-fish-energy-density.html"
  attr(energy_density, "data_files")   <- list(
    energy_density_csv = energy_density_csv)
  attr(energy_density, "data_steward") <- c(
    " Mark Wuenschel <mark.wuenschel@noaa.gov>")
}
get_forage_energy_density(save_clean = T)
