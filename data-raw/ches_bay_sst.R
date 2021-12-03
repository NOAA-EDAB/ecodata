## Chesapeake Bay SST

library(raster)

raw.dir <- here::here("data-raw")

# Chesapeake bay sst files from Ron
ches_1_nc <-"ChesBaySST-seasonal-anomaly_Season1_2021vs2009-2020_RVogel - Ronald Vogel - NOAA Affiliate.nc4"
ches_2_nc <-"ChesBaySST-seasonal-anomaly_Season2_2021vs2009-2020_RVogel - Ronald Vogel - NOAA Affiliate.nc4"
ches_3_nc <-"ChesBaySST-seasonal-anomaly_Season3_2021vs2009-2020_RVogel - Ronald Vogel - NOAA Affiliate.nc4"
ches_4_nc <-"ChesBaySST-seasonal-anomaly_Season4_2021vs2009-2020_RVogel - Ronald Vogel - NOAA Affiliate.nc4"


get_ches_bay_sst <- function(save_clean = F){

  ches1<- raster::raster(file.path(raw.dir, ches_1_nc),varname = "sst_anomaly")
  #raster::crs(ches1) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  #test<- projectRaster(ches1, crs='+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,')
  ches2<- raster::raster(file.path(raw.dir, ches_2_nc),varname = "sst_anomaly")
  ches3<- raster::raster(file.path(raw.dir, ches_3_nc),varname = "sst_anomaly")
  ches4<- raster::raster(file.path(raw.dir, ches_4_nc),varname = "sst_anomaly")

  sst_rast<- raster::stack(ches1, ches2, ches3, ches4)

  # Once run this fails to plot clean
  #crs(sst_rast) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83"

  plot(sst_rast) # plots clean but extent is wild.


  ches_bay_sst <- # dataframe that looks like ecodata::seasonal_sst_anomaly_gridded


  if (save_clean){
    usethis::use_data(ches_bay_sst, overwrite = T)
  } else {
    return(ches_bay_sst)
  }
}
get_ches_bay_sst(save_clean = T)
