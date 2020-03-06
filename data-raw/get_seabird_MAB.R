library(dplyr)
library(tidyr)

#Get raw
raw.dir <- here::here("data-raw") #input raw

get_seabird_mab <- function(save_clean = F){

  d <- read.csv(file.path(raw.dir,"mab_bird.csv"))

  seabird_mab <- d %>%
    dplyr::mutate(EPU = c("MAB"),
           Units = c("Number of breeding pairs"))

  if (save_clean){
    usethis::use_data(seabird_mab, overwrite = T)
  } else {
    return(seabird_mab)
  }

}
get_seabird_mab(save_clean = T)

