## Chesapeake Bay SST gridded

library(raster)
library(tidyverse)
library(stars)
raw.dir <- here::here("data-raw")

# Chesapeake bay sst files from Ron
 ches_1_nc <-"Vogel_ChesBay_SST_Anomaly_2023_Winter - Ronald Vogel - NOAA Affiliate.nc4" #Winter
 ches_2_nc <-"Vogel_ChesBay_SST_Anomaly_2023_Spring - Ronald Vogel - NOAA Affiliate.nc4" #Spring
 ches_3_nc <-"Vogel_ChesBay_SST_Anomaly_2023_Summer - Ronald Vogel - NOAA Affiliate.nc4" #Summer
 ches_4_nc <-"Vogel_ChesBay_SST_Anomaly_2023_Fall - Ronald Vogel - NOAA Affiliate.nc4" #Fall


get_ches_bay_sst <- function(save_clean = F){

  ches1<- raster::raster(file.path(raw.dir, ches_1_nc),varname = "sst_anomaly") #Winter
  ches2<- raster::raster(file.path(raw.dir, ches_2_nc),varname = "sst_anomaly") #Spring
  ches3<- raster::raster(file.path(raw.dir, ches_3_nc),varname = "sst_anomaly") #Summer
  ches4<- raster::raster(file.path(raw.dir, ches_4_nc),varname = "sst_anomaly") #Fall


  sst_rast<- raster::stack(ches1, ches2, ches3, ches4)
  raster::extent(sst_rast) <- raster::extent(c(-77.5,-74.5, 36.5, 40))
  crs(sst_rast) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83"

  r <- as(sst_rast, "SpatialPointsDataFrame") #Convert to ggplot-able object
  r <- as.data.frame(r)

  ches_bay_sst <- r %>%
    dplyr::rename(Winter = "sea_surface_subskin_temperature.anomaly.2023.minus.2007.2022.1",
                  Spring = "sea_surface_subskin_temperature.anomaly.2023.minus.2007.2022.2",
                  Summer = "sea_surface_subskin_temperature.anomaly.2023.minus.2007.2022.3",
                  Fall = "sea_surface_subskin_temperature.anomaly.2023.minus.2007.2022.4") %>%
    tidyr::pivot_longer(!c(x, y), names_to = "Season", values_to = "Value") %>%
    dplyr::rename(Longitude = "y",
                  Latitude = "x",
                  Var = "Season") %>%
    filter(!Value == "NaN")

  # metadata ----
  attr(ches_bay_sst, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/chesapeake-bay-seasonal-sst-anomalies.html"
  attr(ches_bay_sst, "data_files")   <- list(
    ches_1_nc = ches_1_nc,
    ches_2_nc = ches_2_nc,
    ches_3_nc = ches_3_nc,
    ches_4_nc = ches_4_nc)
  attr(ches_bay_sst, "data_steward") <- c(
    "Bruce Vogt <bruce.vogt@noaa.gov>",
    "Ron Vogel <ronald.vogel@noaa.gov>")
  attr(ches_bay_sst, "plot_script") <- list(
    `ltl_MAB` = "LTL_MAB.Rmd-ches-bay-sst.R")


  if (save_clean){
    usethis::use_data(ches_bay_sst, overwrite = T)
  } else {
    return(ches_bay_sst)
  }
}
get_ches_bay_sst(save_clean = T)

















