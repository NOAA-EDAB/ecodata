# Process North Atlantic Oscillation time series

# North Atlantic Oscillation data were taken from the NOAA NWS Climate Prediction Center.
# These data show the monthly NAO index time series beginning in 1950 and ending in November 2018.
# The index is standardized by the standard deviation of the 1950-2000 reference period. More
# information regarding the methodology involved in deriving the NAO and its significance is
# available at http://www.cpc.ncep.noaa.gov/data/teledoc/nao.shtml.

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

#Get raw
raw.dir <- here::here("inst","extdata") #input raw
clean.dir <- here::here("data") #output clean


get_nao <- function(save_clean = F){
  
  nao <- read.table(file.path(raw.dir, "monthly_nao.txt")) %>% 
    dplyr::rename(Time = V1, Month = V2, Value = V3) %>% 
    # mutate(Quarter = plyr::mapvalues(Month, from = c(1:12),
    #                                  to = rep(c("Q1","Q2","Q3","Q4"), each = 3))) %>% 
    # group_by(Year, Quarter) %>% 
    # dplyr::summarise(Value = mean(Value))
    mutate(Units = "unitless", EPU = "All") %>% 
    unite(.,"Time",c("Time","Month"), sep = "-") %>% 
    mutate(Time = as.Date(as.yearmon(Time, "%Y-%m")),
           Var = "north atlantic oscillation") %>% 
    as.data.frame()

  if (save_clean){
    save(nao, file = file.path(clean.dir,"monthly_nao.Rds"))
  } else {
    return(nao)
  }
}
