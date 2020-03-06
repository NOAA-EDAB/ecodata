## HMS Landings

library(tidyverse)


raw.dir <- here::here("data-raw")

get_hms_landings <- function(save_clean = F){

  hms_landings<- read.csv(file.path(raw.dir, "hms_landings.csv"))

  hms_landings<-hms_landings %>%
    tidyr::pivot_longer(cols = starts_with("Landings"),
                        names_to = "Variable", values_to = "Value") %>%
    tidyr::unite(Var,Species, Variable ) %>%
    dplyr::rename(Time = "Year") %>%
    dplyr::mutate(Units = rep(c("US dollars", "lbs"), 74),
                  Region = plyr::mapvalues(Region, from = c("New England", "Mid-Atlantic"),
                                           to = c("NE", "MA")))

  if (save_clean){
    usethis::use_data(hms_landings, overwrite = T)
  } else {
    return(hms_landings)
  }
}
get_hms_landings(save_clean = T)
