## Habitat occupency model for summer flounder

library(dplyr)
library(tidyr)

#Get raw data
raw.dir <- here::here("data-raw")


get_hab_occupancy <- function(save_clean = F){

  d <- read.csv(file.path(raw.dir,"hab_occupancy.csv"))

  #Process
  habitat_occupancy <- d %>%
    rename("Time" = "Year") %>%
    tidyr::gather(key  = "Var", value = "Value", -Time) %>%
    mutate(EPU = c("All"),
           Units = c("10^3km^2"))

  if (save_clean){
    usethis::use_data(habitat_occupancy, overwrite = T)
  } else {
    return(habitat_occupancy)
  }
}
get_hab_occupancy(save_clean = T)
