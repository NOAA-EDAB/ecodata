# Commercial and recreational engagement and reliance

#More information about these data are available at https://noaa-edab.github.io/tech-memo/fishery-reliance-and-social-vulnerability.html

library(dplyr)
library(tidyr)


raw.dir <- here::here("inst","extdata")

get_eng_rel <- function(save_clean = F){
  
  ne <- read.csv(file.path(raw.dir,"Com_n_Rec_Indicators_New_England_121818.csv"))
  mab <- read.csv(file.path(raw.dir,"Com_n_Rec_Indicators_Mid-Atlantic_121818.csv"))
  
  #Process
  eng_rel <- rbind(ne,mab) %>%
    dplyr::select(ComEng_NE16_ct, ComRel_NE16_ct,
                  RecEng_NE16_ct, RecRel_NE16_ct,
                  PRIMARY_LONGITUDE, PRIMARY_LATITUDE,
                  REGION, STATEABBR) %>% 
    mutate(ComEng_NE16_ct = factor(ComEng_NE16_ct))
  
  if (save_clean){
    usethis::use_data(eng_rel, overwrite = T)
  } else {
    return(eng_rel)
  }
  
}




