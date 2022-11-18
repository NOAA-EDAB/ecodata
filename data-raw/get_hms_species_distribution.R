## HMS Species Distribution Shifts
## From paper

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
shift_xlsx <- "HMS_Centroids_data.xlsx"

get_HMS_species_distribution <- function(save_clean = F){

  HMS_species_distribution<- readxl::read_excel(file.path(raw.dir,shift_xlsx))

  HMS_species_distribution<- HMS_species_distribution %>%

    tidyr::pivot_longer( cols = c("wlat","wlon"),
                         names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Var = paste0(Var, "_", species, "_", season),
                  Units = "NA",
                  EPU = "ALL") %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  if (save_clean){
    usethis::use_data(HMS_species_distribution, overwrite = T)
  } else {
    return(HMS_species_distribution)
  }
}
get_HMS_species_distribution(save_clean = T)
