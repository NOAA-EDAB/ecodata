## Aggregate biomass

library(dplyr)


raw.dir <- here::here("data-raw")

get_agg_bio <- function(save_clean = F){

  load(file.path(raw.dir, "Aggregate_Survey_biomass_20 (5).RData"))

  agg_bio <- survey %>%
    rename(EPU = Region)


  if (save_clean){
    usethis::use_data(agg_bio, overwrite = T)
  } else {
    return(agg_bio)
  }

}
get_agg_bio(save_clean = T)
