# Process zooplankton abundance anomalies and small-large index

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")

get_zoo_sli_anom <- function(save_clean = F){

  load(file.path(raw.dir, "zoo_sli_anom.Rdata"))

  zoo_sli_anom <- test %>%
  dplyr::rename(EPU = variable, Value = value)

  if (save_clean){
    usethis::use_data(zoo_sli_anom, overwrite = T)
  } else {
    return(zoo_sli_anom)
  }
}
get_zoo_sli_anom(save_clean = T)
