## HMS Landings

library(tidyverse)


raw.dir <- here::here("data-raw")

get_hms_landings_rev <- function(save_clean = F){

  hms_landings<- read.csv(file.path(raw.dir, "hms_landings.csv"))

  hms_landings_rev <- hms_landings %>%
    dplyr::select(-c("Landings.Weight")) %>%
    dplyr::rename(Time = "Year", Var = "Species", Value = "Landings.Revenue") %>%
    mutate(Units = "US dollars",
           Value = as.numeric(paste(Value)),
           Region = plyr::mapvalues(Region, from = c("New England", "Mid-Atlantic"),
                                 to = c("NE", "MA")))


  if (save_clean){
    usethis::use_data(hms_landings_rev, overwrite = T)
  } else {
    return(hms_landings_rev)
  }
}
get_hms_landings_rev(save_clean = T)
