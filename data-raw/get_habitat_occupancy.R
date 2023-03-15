## Habitat occupency model for summer flounder

library(dplyr)
library(tidyr)

#Get raw data
raw.dir <- here::here("data-raw")
habitat_occupancy_csv<- "hab_occupancy.csv"

get_hab_occupancy <- function(save_clean = F){

  d <- read.csv(file.path(raw.dir,habitat_occupancy_csv))

  #Process
  habitat_occupancy <- d %>%
    dplyr::rename("Time" = "Year") %>%
    tidyr::gather(key  = "Var", value = "Value", -Time) %>%
    dplyr::mutate(EPU = c("All"),
           Units = c("10^3km^2"))%>%
    dplyr::select(Time, Var, Value, EPU) %>%
    tibble::as_tibble()

  if (save_clean){
    usethis::use_data(habitat_occupancy, overwrite = T)
  } else {
    return(habitat_occupancy)
  }
  # metadata ----
  attr(habitat_occupancy, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/hab-occu.html"
  attr(habitat_occupancy, "data_files")   <- list(
    habitat_occupancy_csv = habitat_occupancy_csv)
  attr(habitat_occupancy, "data_steward") <- c(
    "Kevin Friedland <kevin.friedland@noaa.gov>")
}
get_hab_occupancy(save_clean = T)
