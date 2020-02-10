library(dplyr)
library(tidyr)

#Get raw
raw.dir <- here::here("data-raw") #input raw

get_seabird_MAB <- function(save_clean = F){

  d <- read.csv(file.path(raw.dir,"mab_bird.csv"))

  seabird_MAB <- d %>%
    mutate(EPU = c("MAB"),
           Units = c("Number of breeding pairs"))

  if (save_clean){
    usethis::use_data(seabird_MAB, overwrite = T)
  } else {
    return(seabird_MAB)
  }

}
get_seabird_MAB(save_clean = T)

