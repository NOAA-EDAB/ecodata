# Process ocean temperature anomaly data

# These data include in situ regional time series of both surface and bottom water temperature anomalies
# on the Northeast Continental Shelf. Raw data is split into four files by EPU (SS, GOM, GB, and MAB).


library(dplyr)
library(tidyr)
library(lubridate)

get_oceantemp_insitu <- function(save_clean = F){

  #Get raw
  raw.dir <- here::here("inst","extdata") #input raw
  clean.dir <- here::here("data") #output clean
  
  ss <- read.csv(file.path(raw.dir,"EcoSS_core_Ttopbot.csv")) %>% mutate(EPU = "SS")
  gom <- read.csv(file.path(raw.dir,"EcoGoM_core_Ttopbot.csv")) %>% mutate(EPU = "GOM")
  gb <- read.csv(file.path(raw.dir,"EcoGB_core_Ttopbot.csv")) %>% mutate(EPU = "GB")
  mab <- read.csv(file.path(raw.dir,"EcoMAB_core_Ttopbot.csv")) %>% mutate(EPU = "MAB")
  
  ocean_temp_insitu <- rbind(ss, gom, gb, mab) %>% #bind all
    dplyr::rename(Time = decimal.year, Var = variable.name, Value = temperature) %>% #rename
    mutate(Units = "degreesC", Time = as.Date(format(date_decimal(Time), "%Y-%b-%d"), "%Y-%b-%d"),
           Var, Var = plyr::mapvalues(Var, from = c("Tsfc_anom",#Rename variables
                                                    "Tsfc_ref",
                                                    "Tbot_anom",
                                                    "Tbot_ref"),
                                      to = c("sst anomaly in situ",
                                             "reference sst in situ (1981-2010)",
                                             "bottom temp anomaly in situ",
                                             "reference bt in situ (1981-2010)"))) %>% 
    group_by(Time = year(Time), EPU, Var, Units) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    as.data.frame() 
  
  if (save_clean){
    save(ocean_temp_insitu, file =
           file.path(clean.dir, "ocean_temp_insitu.Rds"))
  } else {
    return(ocean_temp_insitu)
  }
}


