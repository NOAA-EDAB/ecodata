# Process in situ ocean salinity anomaly data

# These data include in situ regional time series of both surface and bottom salinity anomalies
# on the Northeast Continental Shelf. Raw data is split into four files by EPU (SS, GOM, GB, and MAB).

library(dplyr)
library(tidyr)
library(lubridate)

#Get raw
raw.dir <- here::here("data-raw") #input raw


get_oceansal_insitu <- function(save_clean = F){
  ss <- read.csv(file.path(raw.dir,"EcoSS_core_Stopbot.csv")) %>% mutate(EPU = "SS")
  gom <- read.csv(file.path(raw.dir,"EcoGoM_core_Stopbot.csv")) %>% mutate(EPU = "GOM")
  gb <- read.csv(file.path(raw.dir,"EcoGB_core_Stopbot.csv")) %>% mutate(EPU = "GB")
  mab <- read.csv(file.path(raw.dir,"EcoMAB_core_Stopbot.csv")) %>% mutate(EPU = "MAB")

  oceansal_insitu <- rbind(ss, gom, gb, mab) %>% #bind all
    dplyr::rename(Time = decimal.year, Var = variable.name, Value = salinity) %>% #rename
    dplyr::mutate(Units = "PSU", Time = lubridate::as.Date(format(date_decimal(Time), "%Y-%b-%d"), "%Y-%b-%d"),
           Var, Var = plyr::mapvalues(Var, from = c("Ssfc_anom",
                                                    "Ssfc_ref",
                                                    "Sbot_anom",
                                                    "Sbot_ref"),
                                      to = c("surface salinity anomaly in situ",
                                             "reference surface salinity in situ (1981-2010)",
                                             "bottom salinity anomaly in situ",
                                             "reference bottom salinity in situ (1981-2010)"))) %>%
    dplyr::group_by(Time = year(Time), EPU, Var, Units) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    as.data.frame()

  if (save_clean){
    usethis::use_data(oceansal_insitu)
  } else {
    return(oceansal_insitu)
  }

}
