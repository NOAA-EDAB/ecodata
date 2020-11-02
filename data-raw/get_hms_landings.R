## HMS Landings

library(tidyverse)


raw.dir <- here::here("data-raw")
hms_landings_csv<-"hms_landings.csv"

get_hms_landings <- function(save_clean = F){

  hms_landings<- read.csv(file.path(raw.dir, hms_landings_csv))

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

  # metadata ----
  attr(hms_landings, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/highly-migratory-species-landings.html"
  attr(hms_landings, "data_files")   <- list(
    hms_landings_csv = hms_landings_csv)
  attr(hms_landings, "data_steward") <- c(
    "Carrie Solatnoff <carrie.solatnoff@noaa.gov>")
}
get_hms_landings(save_clean = T)
