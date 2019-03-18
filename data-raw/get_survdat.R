# Process aggregated survey data (survdat)

# More information about these data are available at https://noaa-edab.github.io/tech-memo/survdat.html


library(dplyr)
library(tidyr)

raw.dir <- here::here("inst","extdata")

load(file.path(raw.dir, "Aggregate_Survey_biomass_19.Rdata"))
  
nefsc_survey <- survey %>% 
    dplyr::rename(EPU = Region) %>% 
    dplyr::select(-Source)  %>%
    distinct() %>% 
    complete(Time = full_seq(min(.$Time):max(.$Time),1),
                                     nesting(EPU,Var))
  
usethis::use_data(nefsc_survey, overwrite = T)
