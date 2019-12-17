## Habitat occupency model for summer flounder

library(dplyr)
library(tidyr)

#Get raw data
raw.dir <- here::here("data-raw")


get_hab_occupency <- function(save_clean = F){

  d <- read.csv(file.path(raw.dir,"hab_occupency.csv"))

  #Process
  habitat_occupency <- d %>%
    rename("Time" = "Year") %>%
    tidyr::gather(key  = "Var", value = "Value", -Time) %>%
    mutate(EPU = c("All"),
           Units = c("10^3km^2"))

  if (save_clean){
    usethis::use_data(habitat_occupency, overwrite = T)
  } else {
    return(habitat_occupency)
  }
}
get_hab_occupency(save_clean = T)
