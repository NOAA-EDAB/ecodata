library(tidyverse)
### File delivered almost unusable to save time edits made in excel
## Orginal File "2021SOE_Forage_ED_summary_Table - Mark Wuenschel - NOAA Federal.csv"


raw.dir <- here::here("data-raw")
energy_density_xlsx<-"2022SOE_Forage_ED_summary_Table.xlsx"
get_forage_energy_density <- function(save_clean = F){

  energy_density <- read_excel(file.path(raw.dir, energy_density_xlsx)) %>%
    dplyr::select(Species, Year, Season, N, "Energy Density (J/GWW)", "StdDev of ED") %>%
    dplyr::rename(Energy.Density_Mean = "Energy Density (J/GWW)",
                  Energy.Density_SD = "StdDev of ED",
                  Time = Year) %>%
    dplyr::mutate(Var = paste0(Species, "/", Season)) %>%
    dplyr::select(-Season, -Species) %>%
    tidyr::pivot_longer(cols = !c(Time,Var),  names_to = "Var2", values_to = "Value" ) %>%
    dplyr::mutate(Var = paste0(Var, "/", Var2),
                  EPU = c("NA")) %>%
    dplyr::select(Time, Var, Value, EPU)

  if (save_clean){
    usethis::use_data(energy_density, overwrite = T)
  } else {
    return(energy_density)
  }
  # metadata ----
  attr(energy_density, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/forage-fish-energy-density.html"
  attr(energy_density, "data_files")   <- list(
    energy_density_xlsx = energy_density_xlsx)
  attr(energy_density, "data_steward") <- c(
    " Mark Wuenschel <mark.wuenschel@noaa.gov>")
}
get_forage_energy_density(save_clean = T)
