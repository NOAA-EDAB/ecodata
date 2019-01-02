# A function to build down-sampled rasters of each Ecological Production Unit for use in masking optimally
# interpolated ecosystem data. 

library(dplyr)
library(tidyr)
library(lubridate)
library(raster)
library(rgdal)
library(sf)

# Directories
gis.dir <- here::here("inst","extdata","gis") #shapefiles for clipping OI data
raw.dir <- here::here("inst","extdata") #input raw
clean.dir <- here::here("data") #output clean

#First function to build EPU rasters
create_epu_mask_oi <- function(EPU){
  
  #Read in EPU shapefile (will be downsampled to match OI raster resolution)
  epu <- readOGR(file.path(gis.dir, "EPU_Extended.shp"), verbose = F) 
  
  #Filter raster by EPU
  epu <- epu[epu$EPU == EPU,]
  
  #Build empty raster
  r1 <- raster::raster()
  e <- raster::extent(-75.950, -65.450, 35.650, 44.650)
  raster::extent(r1) <- e
  
  #fill with EPU polygon
  r1 <- raster::rasterize(epu, r1, field = 1, fun = mean)
  raster::crs(r1) <- NA
  
  #create raster to resample with
  r2 <- raster::raster(nrow = 90, ncol = 105)
  raster::extent(r2) <- e
  raster::crs(r2) <- NA
  
  #Downsample high res EPU raster to match data
  r.new <- raster::resample(r1, r2, method="bilinear")
  r.new[is.finite(r.new)] <- 1 
  
  return(r.new)
  
}

#Raster data
mab_rast <- create_epu_mask_oi(EPU = "MAB")
gb_rast <- create_epu_mask_oi(EPU = "GB")
gom_rast <- create_epu_mask_oi(EPU = "GOM")