#processing AMAPPS harbor porpoise data
library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(here)
library(rgdal)
library(colorRamps)
library(gstat)

gis.dir <- here::here("data-raw","gridded")

crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
hp_denisty_csv<-"Harbor_Porpoise - Fall AMAPPS mapping data from website.csv"
process_hp <- function(season){
  d <- read.csv(file.path(gis.dir,hp_denisty_csv),
                stringsAsFactors = FALSE) %>%
    dplyr::filter(Season == season) %>%
    dplyr::select(Latitude = Center_Lat,
                  Longitude = Center_Lon,
                  Density = Mean_Density) %>%
    dplyr::mutate(Density = as.numeric(Density)) %>%
    dplyr::filter(!is.na(Density))

  #dataframe to spdf
  coordinates(d) <- ~Longitude + Latitude
  crs(d) <- crs
  d.spdf <- as(d, "SpatialPointsDataFrame")

  #spdf to raster to spatial pixels to data frame
  r <- raster(extent(-77,-63,35,45), ncol = 105,nrow = 105)
  crs(r) <- crs
  l <- raster::rasterize(d.spdf, r, fun = mean)[[2]]
  d.df <- as.data.frame(as(l,"SpatialPixelsDataFrame"))

  #cuts to bin data with
  cuts <- c(0, 1e-16, 0.00002, 0.00004,0.00007,0.00015,0.00028,0.00055,0.00108,
            0.00211,0.00411,0.00802,0.01566,0.03057,0.05967,0.11646,0.22732,
            0.44369,0.86601,1.69031,3.29919,7)

  #convert to dataframe for plotting
  hp_density <- d.df %>%
    reshape2::melt(id = c("y","x")) %>%
    dplyr::rename(Latitude = y, Longitude = x) %>%
    dplyr::select(-variable) %>%
    dplyr::mutate(Bin = cut(value, breaks = cuts),
           Season = season) %>%
    dplyr::rename(Value = value)

  return(hp_density)
}

fall_hp <- process_hp(season = "Fall")
spring_hp <- process_hp(season = "Spring")

hp_density <- rbind(fall_hp, spring_hp)


# metadata ----
attr(hp_density, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/harbor-porpoise-bycatch.html"
attr(hp_density, "data_files")   <- list(
  hp_denisty_csv = hp_denisty_csv)
attr(hp_density, "data_steward") <- c(
  "Chris Orphanides <chris.orphanides@noaa.gov>")
usethis::use_data(hp_density, overwrite = T)



