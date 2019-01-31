#Processing for spatial SST anomaly

library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(ncdf4)
library(reshape2)

rast_prep <- function(r){
  r <- rotate(r) #Rotate
  r <- crop(r, extent(-77,-66,35,45)) #Crop
  return(r)
}

raw.dir <- here::here("inst","extdata","gridded")
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40
+lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"


#These data are large files that are not included among ecodata source files. They are accessible
#here: https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html
# sst.2018 <- rast_prep(stack(file.path(raw.dir, "sst.day.mean.2018.nc")))
# ltm <- rast_prep(stack(file.path(raw.dir, "sst.day.mean.ltm.1982-2010.nc")))


winter.ltm <- ltm[[1:90]] 
spring.ltm <- ltm[[91:181]]
summer.ltm <- ltm[[182:273]]
fall.ltm <- ltm[[274:365]]

winter.anom <- sst.2018[[1:90]] - winter.ltm
spring.anom <- sst.2018[[91:181]] - spring.ltm
summer.anom <- sst.2018[[182:273]] - summer.ltm
fall.anom <- sst.2018[[274:365]] - fall.ltm


rast_process <- function(r, season){
  r <- stackApply(r, indices = rep(1,nlayers(r)),mean) #Find mean anomaly
  crs(r) <- crs #Add SOE CRS
  r <- disaggregate(r, 2) #interpolate step 1 - create higher res grid
  r <- focal(r, w=matrix(1, 5, 5), mean) #interpolate step 2 - moving window 
  r <- as(r, "SpatialPointsDataFrame") #Convert to ggplot-able object
  r <- as.data.frame(r)
  r <- r %>%
    reshape2::melt(id = c("y","x")) %>%
    dplyr::rename(Latitude = y, Longitude = x) %>%
    dplyr::select(-variable) %>% 
    mutate(Season = season) %>% 
    dplyr::rename(Value = value)
    
  return(r)
}

seasonal_sst_anomaly <- 
  rbind(rast_process(winter.anom,season = "Winter"),
      rast_process(spring.anom,season = "Spring"),
      rast_process(summer.anom, season = "Summer"),
      rast_process(fall.anom, season = "Fall"))

usethis::use_data(seasonal_sst_anomaly, overwrite = T)




ggplot(data = sst_anomaly) +
  geom_tile(aes(x = Longitude, y = Latitude, fill = value)) +
  facet_wrap(Season~., ncol = 2)



