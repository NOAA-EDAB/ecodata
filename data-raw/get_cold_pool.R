### Cold pool index from Zhuomin Chen

library(dplyr)
library(tidyr)
library(tidync)



raw.dir <- here::here("data-raw")

cold_pool_nc<- "Glorys12v1_ColdPool_Extents.nc" #File from Zhoumin

get_cold_pool_sf <- function(save_clean = F){


  cold_pool_file <- list.files(raw.dir, pattern=cold_pool_nc,full.names=TRUE)

  r <- raster::stack(cold_pool_file)

  min <- 0.00000001
  max <- 2000
  dat<- raster::clamp(r, lower=min, upper=max, useValues=FALSE) # clamp rasters around values
  ## crs = coordinate reference system  - this is the standard we use for mapping the northeast shelf
  crs<- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

  crs(dat)<- crs #sets the crs for the raster data
  nlayers<- c(1:26) # number of years in data
  #i<-16
  main<- NULL
  for (i in nlayers){
    test<- rasterToPolygons(dat[[i]], dissolve = TRUE) #create polygons of clamped rasters
    test2 <- test %>%
      st_as_sf() %>% #make it a spatial feature
      st_union() %>% # merges the rasterized polygon into single cold pool polygon - one for each year
      as_Spatial()
    sff<-data.frame()
    sff<-st_as_sf(test2, coords = c("long", "lat"), crs = crs) #sff$geometry shows correct crs

    main<-rbind(main, sff)
    print(i)

    plot(sff[[1]][[1]])
  }

  cold_pool_shp<- main %>%
    sf::st_transform(crs = crs) #try to set the crs again.
  #st_write(cold_pool_shp,  paste0(raw.dir, "/", "ColdPoolMaps.shp"))

  cold_pool_sf<- as(cold_pool_shp, "sf")

# ignore this if else and the usethis function
  if (save_clean){
    usethis::use_data(cold_pool_sf)
  } else {
    return(cold_pool_sf)
  }
}
#get_cold_pool_sf(save_clean = T, overwrite = TRUE)
usethis::use_data(cold_pool_sf, overwrite = TRUE)

#Trying to make a plot that shows the cold pool in the correct place. should be off the mid-atlantic.

  plot(cold_pool_sf) # This plots with no x or y values but the layered polygons look correct

  ## test plot ---- WRONG BECAUSE IT IS PLOTTING THE COLD POOL IN THE EASTERN HEMISPHERE??? and too big??
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = cold_pool_sf, fill = "transparent", color = "black")












## --------------------------------------------------------------------------------- ##
    ### Cold Pool time series
    ### This code is correct so don't worry about this
get_cold_pool <- function(save_clean = F){
  cold_pool <- tidync::tidync(cold_pool_file) %>%
    tidync::hyper_tibble() %>%
    dplyr::mutate(Time = c(ny+1992)) %>%
    dplyr::filter(V_max < 100,
                  T_peak < 100,
                  T_min < 100,
                  T_mean < 100) %>%
    tidyr::pivot_longer(cols = c("T_peak", "T_min", "T_mean", "V_max"), names_to = "Var",values_to = "Value") %>%
    dplyr::group_by(Time, Var) %>%
    dplyr::summarise(Val = mean(Value),
                     Uncertainty = sd(Value)) %>%
    dplyr::mutate(EPU = c("MAB"))


  if(save_clean){
    usethis::use_data(cold_pool, overwrite = T)
  } else {
    return(cold_pool)
  }
  # metadata ----
  attr(cold_pool, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/cold-pool-index.html"
  attr(cold_pool, "data_files")   <- list(
    cold_pool_nc = cold_pool_nc)
  attr(cold_pool, "data_steward") <- c(
    "Zhuomin Chen <zchen@whoi.edu>")

}
get_cold_pool(save_clean = T)

### Cold pool index from Chris Melrose
# cold_pool_csv<- "cold_pool_index.csv"
#
# get_cold_pool <- function(save_clean = F){
#
#   cold_pool <- read.csv(file.path(raw.dir, cold_pool_csv)) %>%
#     dplyr::rename(EPU = Region,
#                   Time = Year,
#                   Value = VAR)
#
#   if(save_clean){
#     usethis::use_data(cold_pool, overwrite = T)
#   } else {
#     return(cold_pool)
#   }
#   # metadata ----
#   attr(cold_pool, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/cold-pool-index.html"
#   attr(cold_pool, "data_files")   <- list(
#     cold_pool_csv = cold_pool_csv)
#   attr(cold_pool, "data_steward") <- c(
#     "Chris Melrose <chris.melrose@noaa.gov>")
#
# }
# get_cold_pool(save_clean = T)
