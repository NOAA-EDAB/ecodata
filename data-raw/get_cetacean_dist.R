## HMS Species Distribution Shifts
## From paper

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
shift_xlsx <- "HMS_Centroids_data.xlsx"

get_cetacean_dist <- function(save_clean = F){

  cetacean_dist<- readxl::read_excel(file.path(raw.dir,shift_xlsx))

  cetacean_dist<- cetacean_dist %>%

    tidyr::pivot_longer( cols = c("wlat","wlon"),
                         names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Var = paste0(Var, "_", species, "_", season),
                  Units = "NA",
                  EPU = "ALL") %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  if (save_clean){
    usethis::use_data(cetacean_dist, overwrite = T)
  } else {
    return(cetacean_dist)
  }
}
get_cetacean_dist(save_clean = T)
