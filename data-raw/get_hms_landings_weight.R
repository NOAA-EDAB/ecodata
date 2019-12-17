## HMS Landings

library(tidyverse)


raw.dir <- here::here("data-raw")

get_hms_landings_weight <- function(save_clean = F){

  hms_landings<- read.csv(file.path(raw.dir, "hms_landings.csv"))

  hms_landings_weight <- hms_landings %>%
    dplyr::select(-c("Landings.Revenue")) %>%
    dplyr::rename(Time = "Year", Var = "Species", Value = "Landings.Weight") %>%
    mutate(Units = "lbs",
           Value = as.numeric(paste(Value)),
           Region = plyr::mapvalues(Region, from = c("New England", "Mid-Atlantic"),
                                    to = c("NE", "MA")))


  if (save_clean){
    usethis::use_data(hms_landings_weight, overwrite = T)
  } else {
    return(hms_landings_weight)
  }
}
get_hms_landings_weight(save_clean = T)
