### SAFMC managed species presence on the NES

## https://noaa-edab.github.io/tech-doc/safmc-managed-spp.html

library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

get_blue_runner <- function(save_clean = F){

   load(file.path(raw.dir,"Blue_runner_presence.RData"))

   blue_runner<- blue.soe %>%
     dplyr::rename(EPU =  Region)

  if (save_clean){
    usethis::use_data(blue_runner, overwrite = T)
  } else {
    return(blue_runner)
  }

}
get_blue_runner(save_clean = T)
