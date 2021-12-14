#Processing for spatial SST anomaly

library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(ncdf4)
library(reshape2)

rast_prep <- function(r){
  r <- rotate(r) #Rotate
  r <- crop(r, extent(-77,-60,35,46)) #Crop
  return(r)
}

ltm.dir <- here::here("data-raw/gridded/sst")
raw.dir <- here::here("data-raw")
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40
+lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

seasonal_sst_anomaly_gridded_day_nc <-"sst.day.mean.2021.nc"
seasonal_sst_anomaly_gridded_ltm_nc <- "sst.day.mean.ltm.1982-2010.nc"
#These data are large files that are not included among ecodata source files. They are accessible
#here: https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html
# but are removed after use as they are too large to store on github
sst.2019 <- rast_prep(stack(file.path(ltm.dir, seasonal_sst_anomaly_gridded_day_nc)))
ltm <- rast_prep(stack(file.path(ltm.dir, seasonal_sst_anomaly_gridded_ltm_nc)))

winter.ltm <- ltm[[1:90]]
spring.ltm <- ltm[[91:181]]
summer.ltm <- ltm[[182:273]]
fall.ltm <- ltm[[274:365]]

winter.anom <- sst.2019[[1:90]] - winter.ltm
spring.anom <- sst.2019[[91:181]] - spring.ltm
summer.anom <- sst.2019[[182:273]] - summer.ltm
fall.anom <- sst.2019[[274:347]] - fall.ltm ### switched to cover just dates included in 2020.nc up to Dec 13th.


rast_process <- function(r, season){
  r <- raster::stackApply(r, indices = rep(1,nlayers(r)),mean) #Find mean anomaly
  crs(r) <- crs #Add SOE CRS
  ### Remove smoothing steps due to "over smoothing'
  #r <- disaggregate(r, 5) #interpolate step 1 - create higher res grid
  #r <- focal(r, w=matrix(1,nrow=5,ncol=5), fun=mean,
  #           na.rm=TRUE, pad=TRUE) #interpolate step 2 - moving window
  r <- as(r, "SpatialPointsDataFrame") #Convert to ggplot-able object
  r <- as.data.frame(r)
  r <- r %>%
    reshape2::melt(id = c("y","x")) %>%
    dplyr::rename(Latitude = y, Longitude = x) %>%
    dplyr::select(-variable) %>%
    dplyr::mutate(Season = season) %>%
    dplyr::rename(Value = value)

  return(r)
}

seasonal_sst_anomaly_gridded <-
  rbind(rast_process(winter.anom,season = "Winter"),
      rast_process(spring.anom,season = "Spring"),
      rast_process(summer.anom, season = "Summer"),
      rast_process(fall.anom, season = "Fall"))

# metadata ----
attr(seasonal_sst_anomaly_gridded, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/seasonal-sst-anomalies.html"
attr(seasonal_sst_anomaly_gridded, "data_files")   <- list(
  seasonal_sst_anomaly_gridded_day_nc = seasonal_sst_anomaly_gridded_day_nc,
  seasonal_sst_anomaly_gridded_ltm_nc = seasonal_sst_anomaly_gridded_ltm_nc)
attr(seasonal_sst_anomaly_gridded, "data_steward") <- c(
  "Kimberly Bastille <kimberly.bastille@noaa.gov>")
attr(seasonal_sst_anomaly_gridded, "plot_script") <- list(
  `ltl_MAB_shelf` = "LTL_MAB.Rmd-shelf-seasonal-sst-anomaly-gridded.R",
  `ltl_NE` = "LTL_NE.Rmd-seasonal-sst-anomaly-gridded.R")

usethis::use_data(seasonal_sst_anomaly_gridded, overwrite = T)


#### Get Gridded Daily Max values for Marine Heatwaves

mab_peak_hw <- sst.2019[[210]] - summer.ltm## 7/28/2020
gb_peak_hw <- sst.2019[[227]] - summer.ltm## 8/14/2020
gom_peak_hw <- sst.2019[[214]] - summer.ltm## 8/2/2020

rast_process_epu <- function(r, epu){
  r <- raster::stackApply(r, indices = rep(1,nlayers(r)),mean) #Find mean anomaly
  crs(r) <- crs #Add SOE CRS
  ### Remove smoothing steps due to "over smoothing'
  #r <- disaggregate(r, 5) #interpolate step 1 - create higher res grid
  #r <- focal(r, w=matrix(1,nrow=5,ncol=5), fun=mean,
  #           na.rm=TRUE, pad=TRUE) #interpolate step 2 - moving window
  r <- as(r, "SpatialPointsDataFrame") #Convert to ggplot-able object
  r <- as.data.frame(r)
  r <- r %>%
    reshape2::melt(id = c("y","x")) %>%
    dplyr::rename(Latitude = y, Longitude = x) %>%
    dplyr::select(-variable) %>%
    dplyr::mutate(EPU = epu) %>%
    dplyr::rename(Value = value)

  return(r)
}


heatwave_peak_date <-
  rbind(rast_process_epu(mab_peak_hw,epu = "MAB"),
        rast_process_epu(gb_peak_hw ,epu = "GB"),
        rast_process_epu(gom_peak_hw, epu = "GOM"))

usethis::use_data(heatwave_peak_date, overwrite = T)
