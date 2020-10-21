## Heatwave maps


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

raw.dir <- here::here("data-raw/gridded/sst_data")
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40
+lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
heatwave_anom_gridded_day_nc <- "sst.day.mean.2019.nc"
heatwave_anom_gridded_ltm_nc <- "sst.day.mean.ltm.1982-2010.nc"
#These data are large files that are not included among ecodata source files. They are accessible
#here: https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html
# but are removed after use as they are too large to store on github
sst.2019 <- rast_prep(stack(file.path(raw.dir, heatwave_anom_gridded_day_nc)))
ltm <- rast_prep(stack(file.path(raw.dir, heatwave_anom_gridded_ltm_nc)))

## Use day of Max heatwave intensity.
gb.ltm <- ltm[[235]] # Aug 23
gom.ltm <- ltm[[213]] # Aug 1
mab.ltm <- ltm[[203]] # July 22

gb.anom<-sst.2019[[235]] - gb.ltm
gom.anom<-sst.2019[[213]] - gom.ltm
mab.anom<-sst.2019[[203]] - mab.ltm

rast_process <- function(r, epu){
  r <- raster::stackApply(r, indices = rep(1,nlayers(r)),mean) #Find mean anomaly
  crs(r) <- crs #Add SOE CRS
  r <- raster::disaggregate(r, 5) #interpolate step 1 - create higher res grid
  r <- raster::focal(r, w=matrix(1,nrow=5,ncol=5), fun=mean,
             na.rm=TRUE, pad=TRUE) #interpolate step 2 - moving window
  r <- as(r, "SpatialPointsDataFrame") #Convert to ggplot-able object
  r <- as.data.frame(r)
  r <- r %>%
    reshape2::melt(id = c("y","x")) %>%
    dplyr::rename(Latitude = y, Longitude = x) %>%
    dplyr::select(-variable) %>%
    dplyr::mutate(EPU = c(epu)) %>%
    dplyr::rename(Value = value)

  return(r)
}

heatwave_anom_gridded<-
  rbind(rast_process(gb.anom, epu = "GB"),
        rast_process(gom.anom,epu = "GOM"),
        rast_process(mab.anom, epu = "MAB"))

# metadata ----
attr(heatwave_anom_gridded, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/marine-heatwave.html"
attr(heatwave_anom_gridded, "data_files")   <- list(
  heatwave_anom_gridded_day_nc = heatwave_anom_gridded_day_nc,
  heatwave_anom_gridded_ltm_nc = heatwave_anom_gridded_ltm_nc)
attr(heatwave_anom_gridded, "data_steward") <- c(
  "Kimberly Bastille <kimberly.bastille@noaa.gov>")
usethis::use_data(heatwave_anom_gridded, overwrite = T)
