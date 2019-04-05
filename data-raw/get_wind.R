# Process raw wind data 
# 
# These data are sourced from the NCEP North American Regional Reanalysis (NARR)
# (https://www.esrl.noaa.gov/psd/data/gridded/data.narr.monolevel.html), extending from January 1979
# to September 2018. 
# 
# 
# Variables included in these data are surface wind speed and direction ("uwnd" and "vwnd" respectively), 
# surface turbulent kinetic energy (TKE), and storm relative helicity (HLCY). An indicator for total
# wind speed is calculated as TWS = sqrt(u^2 + v^2). Data are visualized seasonally (Fall = October, 
# November, December; Winter = January, February, March; Spring = April, May, June; Summer = July,
# August, September).

library(dplyr)
library(tidyr)
library(lubridate)

#Get raw
raw.dir <- here::here("data-raw") #input raw

get_wind <- function(save_clean = F){
  
  
  d <- read.csv(file.path(raw.dir,"NCEP NARR surface wind; TKE; HLCY, monthly, 1979-2018, V1.csv"))
  
  
  # Processing steps for all wind speed data
  wind_clean1 <- d  %>% gather(., Var, Value, GB.uwnd:MAB.tke) %>% #convert wide to long
    dplyr::rename(Time = Month.Year) %>% #rename time variable
    separate(Var, c("EPU","Var"),"\\.") %>% #separate out EPU from variable names
    mutate(Time = dmy(.$Time), #Convert to date format
                  Units = plyr::mapvalues(Var, from = unique(Var), to = c(rep("J/kg",2),"m^2/sec^2","J/kg")), #add units
                  Time, season = plyr::mapvalues(month(Time), from = seq(1,12,1), #Get season
                                                 to = c(rep("winter",3),
                                                        rep("spring",3),
                                                        rep("summer",3),
                                                        rep("fall",3)))) 
  # Calculate total wind speed from u and v components
  total_wind_speed <- wind_clean1 %>% 
    filter(Var == "uwnd"| Var == "vwnd") %>% #select variables
    spread(., Var, Value) %>% #convert to wide for calculating tws
    mutate(`total wind speed` = sqrt(uwnd^2 + vwnd^2)) %>%  #tws
    dplyr::select(-uwnd, -vwnd) %>% #start processing back to SOE format
    gather(.,Var, Value, `total wind speed`) #convert to long
  
  wind_clean <- rbind(wind_clean1, total_wind_speed)
  ne_wind <- wind_clean %>%
    unite(., Var, c(Var, season), sep = " ") %>% #merge season into Var column
    group_by(Time = year(Time), EPU, Var, Units) %>% 
    dplyr::summarise(Value = mean(Value)) %>% 
    as.data.frame()
  
  if (save_clean){
    usethis::use_data(ne_wind, overwrite = T)
  } else {
    return(wind)
  }
  
}
