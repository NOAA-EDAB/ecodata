library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)


raw.dir <- here::here("inst","extdata")


ppd <- read.csv(file.path(raw.dir,"SOE_V2019_1-NES_ECOREGIONS-PPD-STATS_ANOMS-SEAWIFS_MODIS.csv")) %>% 
  mutate(ALGORITHM = word(str_replace(ALGORITHM, "_", " "))) %>%
  unite(.,VARIABLE, c("VARIABLE","SENSOR","ALGORITHM"), sep = " ") %>%
  mutate(VARIABLE = ifelse(str_detect(FILENAME, "1998_2018"), paste(VARIABLE,"1998_2018"),
                           ifelse(str_detect(FILENAME, "1998_2017"), paste(VARIABLE, "1998_2017"),
                                  ifelse(str_detect(FILENAME, "1997_2018"), paste(VARIABLE, "1997_2018"),
                                         ifelse(str_detect(FILENAME, "1997_2017"), paste(VARIABLE, "1997_2017"),
                                                VARIABLE))))) %>%
  dplyr::select(TIME, UNITS, VARIABLE, VALUE, REGION) %>%
  dplyr::rename(Time = TIME, Units = UNITS, Var = VARIABLE,
                EPU = REGION, Value = VALUE)

chl <- read.csv(file.path(raw.dir,"SOE_V2019_1-NES_ECOREGIONS-CHLOR_A-STATS_ANOMS-SEAWIFS_MODIS.csv")) %>% 
  mutate(ALGORITHM = word(str_replace(ALGORITHM, "_", " "))) %>%
  unite(.,VARIABLE, c("VARIABLE","SENSOR","ALGORITHM"), sep = " ") %>%
  mutate(VARIABLE = ifelse(str_detect(FILENAME, "1998_2018"), paste(VARIABLE,"1998_2018"),
                           ifelse(str_detect(FILENAME, "1998_2017"), paste(VARIABLE, "1998_2017"),
                                  ifelse(str_detect(FILENAME, "1997_2018"), paste(VARIABLE, "1997_2018"),
                                         ifelse(str_detect(FILENAME, "1997_2017"), paste(VARIABLE, "1997_2017"),
                                                VARIABLE))))) %>%
  dplyr::select(TIME, UNITS, VARIABLE, VALUE, REGION) %>%
  dplyr::rename(Time = TIME, Units = UNITS, Var = VARIABLE,
                EPU = REGION, Value = VALUE)


chl_pp <- rbind(ppd,chl)

usethis::use_data(chl_pp, overwrite = T)

