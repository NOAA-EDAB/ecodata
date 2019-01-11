# A function to process and make ecological production unit (EPU) shapefiles available in the ecodata package. 

# Read more about the delineation of EPUs at https://noaa-edab.github.io/tech-memo/epu.html


library(sf)
library(rgdal)
library(raster)
library(rnaturalearth)



gis.dir <- here::here('inst','extdata','gis')
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77+x_0=0
+y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

get_epu_sf <- function(save_clean){
  
  epu_shp <- readOGR(file.path(gis.dir, "EPU_Extended.shp"), verbose = F) 
  crs(epu_shp) <- crs
  epu_sf <- as(epu_shp, "sf")
  
  if (save_clean){
    usethis::use_data(epu_sf)
  } else {
    return(epu_sf)
  }
}