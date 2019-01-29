# Processing for EcoMon nutrient and oceanographic data

# These data include nutrient concentrations, temperature, salinity, density, and dissolved oxygen
# data sampled via CTD profiles on Ecosystem Monitoring (EcoMon) cruises between 11/3/2009 - 10/19/2016.
# More metadata are available at https://www.nodc.noaa.gov/oads/data/0127524.xml

# Variable definitions

# vars <- data.frame(Variable = c("Cruise identifier","Cruise identifier","Station number",
#                                 "CTD cast number","Sample bottle number","Sample date",
#                                 "Sample time","Latitude","Longitude","Depth of station",
#                                 "Depth of sample","Water pressure","Water temperature",
#                                 "Water salinity","Potential density at surface pressure",
#                                 "Dissolved oxygen","Silicic acid concentration",
#                                 "Total nitrate and nitrite concentration","Ammonia concentration",
#                                 "Phosphate concentration","Dissolved oxygen"),
#                    Names = c("EXPOCODE","Cruise_ID","STNNBR","CASTNO",
#                              "BTLNBR","Date_UTC","Time_UTC",
#                              "Latitude","Longitude","Depth_station",
#                              "Depth_sampling","CTDPRS","CTDTEMP",
#                              "CTDSAL","Sigma.Theta","CTDOXY",
#                              "SILCAT","NITRIT+NITRAT","AMMMONIA",
#                              "PHSPHT","CTDOXYMOL"),
#                    Units = c("","","",
#                              "","","MM/DD/YYYY",
#                              "hh:mm","decimal degrees","decimal degrees",
#                              "m","m","decibars","°C",
#                              "PSS-78","kg m^-3^","mg L^-1^",
#                              "$\\mu$M","$\\mu$M","$\\mu$M",
#                              "$\\mu$M","$\\mu$mol kg^-1^"))

library(dplyr)
library(tidyr)
library(rgdal)
library(raster)
library(sf)
library(lubridate)

#Data directories
raw.dir <- here::here("inst","extdata")
gis.dir <- here::here("inst","extdata","gis")

#CRS
crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

get_ecomon <- function(save_clean = F){
  
  d <- read.csv(file.path(raw.dir,"EcoMon Nutrient Data Through June 2018.csv"), stringsAsFactors = FALSE)
  
  #Create data frame for mapping units to variable names
  mapping <- data.frame(Units = as.character(d[1,]),
                        Var = as.character(names(d)))
  mapping[mapping$Units == "" | mapping$Units == "NA",]$Units <- NA
  
  #remove row with units
  d <- slice(d,-1)
  
  d1 <- d %>% 
    mutate(Time = Date_UTC) %>% #create Time variable
    dplyr::select(-Date_UTC,-Time_UTC) %>% #remove time, date
    gather(., Var, Value, -Latitude, -Longitude, -Time, -Depth_sampling, -Depth_station) %>% #turn wide to long while retaining lat/lon
    filter(!is.na(Value)) %>% #remove NA
    left_join(., mapping, by = c("Var")) %>% #join units 
    mutate(Longitude = as.numeric(Longitude),
           Latitude = as.numeric(Latitude),
           Time = mdy(Time)) %>% 
    filter(Latitude > 32, Latitude<50)
  
  #Read in EPU shapefile
  epu <- readOGR(file.path(gis.dir, "EPU_Extended.shp"), verbose = F) 
  epu <- as(epu, "sf") #convert to sf object
  
  #get latitude and longitude for creating SpatialPointsDataFrame
  lat <-  as.numeric(d$Latitude)
  lon <- as.numeric(d$Longitude)
  coords <- data.frame(lon = lon,lat = lat)
  
  #create spdf
  spdf <- SpatialPointsDataFrame(coords = coords, data = coords,
                                 proj4string = CRS(crs))
  #convert to sf
  coords_sf <- st_as_sf(spdf) 
  
  #get intersection for mapping EPUs back to nutrient data
  epu_intersect <- st_intersection(epu, coords_sf)
  
  #Map back to nutrient data frame
  epu_df <- data.frame(Longitude = epu_intersect$lon,
                       Latitude = epu_intersect$lat,
                       EPU = epu_intersect$EPU)
  #join
  ecomon <- d1 %>% 
    left_join(.,epu_df, by = c("Latitude","Longitude")) %>% 
    filter(!Var %in% c("Cruise_ID","EXPOCODE")) %>% 
    dplyr::select(-Latitude, -Longitude) %>% 
    mutate(Value = as.numeric(Value),
           Depth_station = as.numeric(Depth_station),
           Depth_sampling = as.numeric(Depth_sampling)) %>% 
    mutate(bot_dif = Depth_station-Depth_sampling) %>% 
    mutate(surf_bot = ifelse(bot_dif <= 10, "bottom",
                             ifelse(bot_dif > 10 & Depth_sampling <= 5, "surface", "mid-water"))) %>% 
    filter(Value > 0, !is.na(EPU), !Var %in% c("BTLNBR","CASTNO","Depth_sampling",
                                               "Depth_station","STNNBR")) %>% 
    mutate(Var = paste(Var, surf_bot)) %>% 
    dplyr::select(Time, Var, Value, Units, EPU) %>% 
    group_by(EPU, Time = year(Time), Var, Units) %>% 
    dplyr::summarise(Value = mean(Value, na.rm = TRUE)) %>% 
    as.data.frame()
  
    if (save_clean){
      usethis::use_data(ecomon, overwrite = T)
    } else {
      return(ecomon)
    }
}

