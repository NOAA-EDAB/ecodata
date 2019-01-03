# Process commerical data


# These data were derived from the Commercial Fisheries Database Biological Sample. More information about
# these data are available at https://noaa-edab.github.io/tech-memo/comdat.html

library(dplyr)


raw.dir <- here::here("inst","extdata")
clean.dir <- here::here('data')

get_comdat <- function(save_clean = F){
  
  load(file.path(raw.dir,"Commercial_data_pull_19.RData"))
  
  commercial <- commercial %>% 
    dplyr::rename(EPU = Region) %>% 
    dplyr::select(-Source)
  
  if (save_clean){
    save(commercial, file = file.path(clean.dir, "comdat.Rds"))
  } else {
    return(commercial)
  }

}



