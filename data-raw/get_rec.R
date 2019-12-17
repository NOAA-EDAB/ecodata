# Processing for recreational fishing indicators

library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

get_rec <- function(save_clean = F){

  files = list.files(raw.dir, pattern = "REC_HARVEST|Rec_angler|Rec_Species")
  for (i in 1:length(files)) assign(files[i], read.csv(file.path(raw.dir,files[i])))

  recdat <- NULL
  for (i in ls()){
    if (stringr::str_detect(i, "REC_|Rec_")){
      d <- get(i) %>%
        dplyr::select(Time, EPU = Region, Value, Units, Var)

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
get_rec(save_clean = T)
