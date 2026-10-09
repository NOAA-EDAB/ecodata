library(tidyverse)
library(readxl)

raw.dir <- here::here("data-raw")
energy_density_csv <- "2026_SOE_line_plot_data - Joseph Warren - NOAA Affiliate.csv"

get_forage_energy_density <- function(save_clean = F) {
  energy_density <-
    read.csv(file.path(raw.dir, energy_density_csv)) %>%
    dplyr::mutate(
      SPECIES = dplyr::recode(SPECIES, "Loligo" = "Loligo squid")
    ) %>%
    dplyr::mutate(SPECIES = dplyr::recode(SPECIES, "Illex" = "Illex squid")) %>%
    dplyr::rename(
      Species = "SPECIES",
      Energy.Density_Mean = "Energy.Density",
      Energy.Density_SD = "StdDev.of.Energy.Density",
      Time = Year
    ) %>%
    dplyr::select(
      Species,
      Time,
      Season,
      N,
      Energy.Density_Mean,
      Energy.Density_SD
    ) %>%
    dplyr::mutate(
      Var = paste0(Species, "/", Season),
      Energy.Density_Mean = as.numeric(Energy.Density_Mean),
      Energy.Density_SD = as.numeric(Energy.Density_SD),
      N = as.numeric(N),
      Time = as.numeric(Time)
    ) %>%
    dplyr::select(-Season, -Species) %>%
    #tidyr::pivot_longer(cols = !c(Time,Var),  names_to = "Var2", values_to = "Value" ) %>%
    tidyr::pivot_longer(
      !c(Time, Var),
      names_to = "Var2",
      values_to = "Value"
    ) %>%
    dplyr::mutate(Var = paste0(Var, "/", Var2), EPU = c("NA")) %>%
    dplyr::select(Time, Var, Value, EPU) %>%
    dplyr::filter(!Time == "NA")

  expanded <- expand.grid(
    Time = min(energy_density$Time):max(energy_density$Time),
    Var = unique(energy_density$Var),
    EPU = "NA"
  )

  energy_density <- full_join(energy_density, expanded)

  if (save_clean) {
    usethis::use_data(energy_density, overwrite = T)
  } else {
    return(energy_density)
  }
}
get_forage_energy_density(save_clean = T)
