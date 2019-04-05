#Aggregate species distribution metrics for the Northeast Shelf

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")

get_species_dist <- function(save_clean = F){

  species_dist <- read.csv(file.path(raw.dir, "sp dist.csv"))  %>% 
    dplyr::rename(depth = DEPTH,
                  Latitude = LAT,
                  Longitude = LON,
                  `along-shelf distance` = ASD,
                  `distance to coast` = DTC,
                  Time = Year) %>% 
    gather(.,Var,Value,-Time) %>% 
    mutate(EPU = "All",
           Units = ifelse(str_detect(Var,"distance"),"km",
                          ifelse(str_detect(Var,"Latitude"),
                                     "degreesN",ifelse(str_detect(Var,"Longitude"),
                                                      "degreesW",ifelse(str_detect(Var, "depth"),
                                                                        "m",NA)))))
  if (save_clean){
    usethis::use_data(species_dist, overwrite = T)
  } else {
    return(species_dist)
  }
}