# Processing for recreational fishing indicators

library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("inst","extdata")
clean.dir <- here::here("data")

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
    save(recdat, file = file.path(clean.dir, "recreational.Rds"))
  } else {
    return(recdat)
  }
}

