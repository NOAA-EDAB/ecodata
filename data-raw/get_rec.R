# Processing for recreational fishing indicators

library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("inst","extdata")

get_rec <- function(save_clean = F){
  
  files = list.files(raw.dir, pattern = "REC_HARVEST|Rec_participants|Rec_angler")
  for (i in 1:length(files)) assign(files[i], read.csv(file.path(raw.dir,files[i])))
  
  recdat <- NULL
  for (i in ls()){
    if (stringr::str_detect(i, "REC_HARVEST|Rec_participants|Rec_angler")){
      d <- get(i) %>%
        dplyr::select(-X, -Source) %>% 
        dplyr::rename(EPU = Region)
      
      assign('recdat',rbind(recdat, d))
      
    }
  }
  
  recdat <- recdat %>% filter(!is.na(EPU))
  
  if (save_clean){
    usethis::use_data(recdat, overwrite = T)
  } else {
    return(recdat)
  }
}
