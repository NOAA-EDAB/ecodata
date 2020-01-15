# Process Bennet indicator; price and volume indicators

library(dplyr)
library(tidyr)
library(magrittr)

raw.dir <- here::here('data-raw')

get_bennet <- function(save_clean = F){

  load(file.path(raw.dir, "Bennet_Index_20.Rdata"))
  bennet <- bennet %>%
    rename(EPU = Region)

  if (save_clean){
    usethis::use_data(bennet, overwrite = T)
  } else {
    return(bennet)
  }

}
get_bennet(save_clean = T)
