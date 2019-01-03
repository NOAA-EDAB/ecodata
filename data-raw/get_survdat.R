# Process aggregated survey data (survdat)

# More information about these data are available at https://noaa-edab.github.io/tech-memo/survdat.html


library(dplyr)
library(tidyr)

raw.dir <- here::here("inst","extdata")

get_survdat <- function(save_clean = F){
  load(file.path(raw.dir, "Aggregate_Survey_biomass_19.Rdata"))
  
  nefsc_survey <- survey %>% 
    dplyr::rename(EPU = Region) %>% 
    dplyr::select(-Source)
  
  if (save_clean){
    usethis::use_data(nefsc_survey, overwrite = T)
  } else {
    return(nefsc_survey)
  }
  
}


