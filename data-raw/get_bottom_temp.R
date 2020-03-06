# Process ocean temperature anomaly data

# These data include in situ regional time series of
# both surface and bottom water temperature anomalies
# on the Northeast Continental Shelf. Raw data is split
# into four files by EPU (SS, GOM, GB, and MAB).


library(dplyr)
library(tidyr)
library(lubridate)

#Get raw
raw.dir <- here::here("data-raw") #input raw

get_bottom_temp <- function(save_clean = F){

  ss <- read.csv(file.path(raw.dir,"bot_temp_SS.csv")) %>% mutate(EPU = "SS")
  gom <- read.csv(file.path(raw.dir,"bot_temp_GOM.csv")) %>% mutate(EPU = "GOM")
  gb <- read.csv(file.path(raw.dir,"bot_temp_GB.csv")) %>% mutate(EPU = "GB")
  mab <- read.csv(file.path(raw.dir,"bot_temp_MAB.csv")) %>% mutate(EPU = "MAB")

  bottom_temp <- rbind(ss, gom, gb, mab) %>% #bind all
    dplyr::mutate(Units = "degreesC", Time = as.Date(format(date_decimal(Time), "%Y-%b-%d"), "%Y-%b-%d"),
           Var, Var = plyr::mapvalues(Var, from = c("Tsfc_anom",#Rename variables
                                                    "Tsfc_ref",
                                                    "Tbot_anom",
                                                    "Tbot_ref"),
                                      to = c("sst anomaly in situ",
                                             "reference sst in situ (1981-2010)",
                                             "bottom temp anomaly in situ",
                                             "reference bt in situ (1981-2010)"))) %>%
    dplyr::group_by(Time = year(Time), EPU, Var, Units) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    as.data.frame()

  if (save_clean){
    usethis::use_data(bottom_temp, overwrite = T)
  } else {
    return(bottom_temp)
  }
}
get_bottom_temp(save_clean = T)
