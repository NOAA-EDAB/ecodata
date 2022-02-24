## Chesapeake Bay SST gridded

library(raster)
library(tidyverse)

raw.dir <- here::here("data-raw")

# Chesapeake bay sst files from Ron
ches_1_nc <-"ChesBaySST-seasonal-anomaly_Season1_2021vs2009-2020_RVogel - Ronald Vogel - NOAA Affiliate.nc4"
ches_2_nc <-"ChesBaySST-seasonal-anomaly_Season2_2021vs2009-2020_RVogel - Ronald Vogel - NOAA Affiliate.nc4"
ches_3_nc <-"ChesBaySST-seasonal-anomaly_Season3_2021vs2009-2020_RVogel - Ronald Vogel - NOAA Affiliate.nc4"
ches_4_nc <-"ChesBaySST-seasonal-anomaly_Season4_2021vs2009-2020_RVogel - Ronald Vogel - NOAA Affiliate.nc4"


get_ches_bay_sst <- function(save_clean = F){

  ches1<- raster::raster(file.path(raw.dir, ches_1_nc),varname = "sst_anomaly")
  ches2<- raster::raster(file.path(raw.dir, ches_2_nc),varname = "sst_anomaly")
  ches3<- raster::raster(file.path(raw.dir, ches_3_nc),varname = "sst_anomaly")
  ches4<- raster::raster(file.path(raw.dir, ches_4_nc),varname = "sst_anomaly")

  sst_rast<- raster::stack(ches1, ches2, ches3, ches4)
  raster::extent(sst_rast) <- raster::extent(c(-77.5,-74.5, 36.5, 40))
  crs(sst_rast) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83"

  r <- as(sst_rast, "SpatialPointsDataFrame") #Convert to ggplot-able object
  r <- as.data.frame(r)

  ches_bay_sst <- r %>%
    dplyr::rename(Winter = "sst.anomaly.2021.minus.2009.2020.1",
                  Spring = "sst.anomaly.2021.minus.2009.2020.2",
                  Summer = "sst.anomaly.2021.minus.2009.2020.3",
                  Fall = "sst.anomaly.2021.minus.2009.2020.4") %>%
    tidyr::pivot_longer(!c(x, y), names_to = "Season", values_to = "Value") %>%
    dplyr::rename(Longitude = "y",
                  Latitude = "x") %>%
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

















