#### NE Shelf Functional Trait Indicators

library(dplyr)
library(tidyr)

# Set directory
raw.dir <- here::here("data-raw")

# Define input files
finfish_traits_csv <- "cwmtraits_forSOE - Bart DiFiore.csv"

get_finfish_traits <- function(save_clean = F){

  finfish_units<-data.frame("Var" = c("trophic_level","offspring_size","age_maturity","length_maturity",
                                      "fecundity","l_inf","k","max_obs_length","PC1","PC2","PC3"),
                            "Units" = c(NA,"mm","years","cm",NA,NA,NA,NA,NA,NA,NA))

  finfish_traits <- read.csv(file.path(raw.dir, finfish_traits_csv)) |>
    tidyr::pivot_longer(cols = -c(region, est_year, season), names_to = "Var", values_to = "Value") |>
    dplyr::left_join(finfish_units) |>
    dplyr::mutate(Var = paste0(season, "-", Var)) |>
    dplyr::rename("Time" = "est_year",
                  "EPU" = "region") |>
    dplyr::select(Time, Var, Value, EPU, Units)


  if (save_clean){
    usethis::use_data(finfish_traits, overwrite = T)
  } else {
    return(finfish_traits)
  }
}
get_finfish_traits(save_clean = T)
