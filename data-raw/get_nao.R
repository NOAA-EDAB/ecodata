# Process North Atlantic Oscillation time series


library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

#Get raw
raw.dir <- here::here("data-raw") #input raw


get_nao <- function(save_clean = F){
  
  nao <- read.csv(file.path(raw.dir, "nao_station_djfm.csv")) %>% 
    dplyr::rename(Time = Hurrell.Station.Based.DJFM.NAO.Index,
                  Value = X) %>% 
    mutate(Var = "north atlantic oscillation",
           Units = "unitless",
           EPU = "All") %>% 
    as.data.frame()

  if (save_clean){
    usethis::use_data(nao, overwrite = T)
  } else {
    return(nao)
  }
}