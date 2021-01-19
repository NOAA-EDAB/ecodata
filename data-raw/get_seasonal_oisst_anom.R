#Analysis of OISST V2 data to extract seasonal SST time series
## Data gathered here
# "https://www.esrl.noaa.gov/psd/cgi-bin/DataAccess.pl?DB_dataset=
# NOAA+High-resolution+Blended+Analysis&DB_variable=Sea+Surface+
# Temperature&DB_statistic=Mean&DB_tid=81047&DB_did=132&DB_vid=2423"
# Spatial paraters
#lat:Begin (36N), End: (46N)
#lon:Begin (-77W), End: (-65W)

library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(ncdf4)
library(reshape2)
library(ecodata)
library(stringr)


raw.dir <- here::here("data-raw","gridded","sst_data")
ltm.dir <- here::here("data-raw","gridded")

epu <- ecodata::epu_sf %>%
  filter(EPU != "SS")


seasonal_epu_ltm <- function(ltm, epu_name){
  ltm <- mask(ltm, epu[epu$EPU == epu_name,])
  ltm_out <- mean(ltm@data@values, na.rm = T)
  return(ltm_out)
}

seasonal_oisst_anom_nc <-"sst.day.mean.ltm.1982-2010.nc"

#Get long-term mean for anomaly calculation
ltm <- raster::stack(file.path(ltm.dir,seasonal_oisst_anom_nc))
ltm <- raster::crop(ltm, extent(280,300,30,50))
ltm <- raster::rotate(ltm)

winter.ltm <- ltm[[1:90]]
winter.ltm <- raster::stackApply(winter.ltm, indices = rep(1,nlayers(winter.ltm)),mean)

spring.ltm <- ltm[[91:181]]
spring.ltm <- raster::stackApply(spring.ltm, indices = rep(1,nlayers(spring.ltm)),mean)

summer.ltm <- ltm[[182:273]]
summer.ltm <- raster::stackApply(summer.ltm, indices = rep(1,nlayers(summer.ltm)),mean)

fall.ltm <- ltm[[274:365]]
fall.ltm <- raster::stackApply(fall.ltm, indices = rep(1,nlayers(fall.ltm)),mean)


#Function to get seasonal averages by year

get_group_mean <- function(fname, epu_name, anom = T){

  #Import raster data
  raw <- raster::stack(file.path(raw.dir, fname))

  crs(raw) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

  #Get layer index and map to year
  message('Getting index')
  year <- NULL
  for (i in 1:nlayers(raw)){
    assign("year",rbind(year, data.frame(Time = str_extract(raw[[i]]@data@names,"\\d{4}"))))
  }

  year_split <- year %>%
    dplyr::group_by(Time) %>%
    dplyr::mutate(day = 1:n()) %>%
    dplyr::mutate(leap = ifelse(n() == 365,"common","leap")) %>%
    dplyr::group_by(leap) %>%
    dplyr::mutate(season = ifelse(day <= 90 & leap == "common", "winter",
                               ifelse(day > 90 & day <= 181 & leap == "common", "spring",
                                      ifelse(day > 181 & day <= 273 & leap == "common", "summer",
                                             ifelse(day > 273 & leap == "common", "fall",

                        ifelse(day <= 91 & leap == "leap", "winter",
                               ifelse(day > 91 & day <= 181 & leap == "leap", "spring",
                                      ifelse(day > 181 & day <= 273 & leap == "leap", "summer",
                                             ifelse(day > 273 & leap == "leap", "fall",NA))))))))) %>%
    dplyr::group_by(Time, leap, season) %>%
    dplyr::mutate(index = paste(Time, season))

  if (any(is.na(year_split))){
    message("NA in year")
  }

  #Rotate from 0-360 to -180-180
  message(paste('Rotating',fname))
  raw1 <- rotate(raw)

  #Split data on layer index - stackApply will break if there are too many layers
  g1 <- year_split %>%
    dplyr::filter(index %in% unique(.$index)[1:10]) %>%
    pull(index)

  g2 <- year_split %>%
    dplyr::filter(!index %in% unique(.$index)[1:10]) %>%
    pull(index)

  #Apply and combine
  message(paste('Finding mean'))
  n <- raster::nlayers(raw1)
  rawMean1 <- raster::stackApply(raw1[[1:length(g1)]], indices = g1, mean)
  rawMean2 <- raster::stackApply(raw1[[(length(g1) + 1):n]], indices = g2, mean)
  rawMean <- raster::stack(rawMean1,rawMean2)

  #Mask output to EPU
  message(paste('Masking to',epu_name))
  out <- raster::mask(rawMean, epu[epu$EPU == epu_name,])

  #Find seasonal anomaly
  mean_sst <- NULL
  for (i in 1:nlayers(out)){

    if (anom){
      season <- str_extract(names(out[[i]]),"winter|spring|summer|fall")
      message(paste('Finding',season,'SST anomaly for',epu_name))
      sst <- mean(out[[i]]@data@values, na.rm = T) - seasonal_epu_ltm(ltm = get(paste0(season,".ltm")),
                                                                      epu = epu_name)
      var <- "anomaly"
    } else {
      sst <- mean(out[[i]]@data@values, na.rm = T)
      var <- "absolute"
    }

    year = out@data@names[i]
    df <- data.frame(Value = sst,
                     year = year,
                     EPU = epu_name,
                     Var = var)

    assign('mean_sst',rbind(mean_sst, df))
  }
  return(mean_sst)
}

#get sst
fname <- list.files(raw.dir)

MAB <- NULL
GOM <- NULL
GB <- NULL

epu_list <- c("MAB","GOM","GB")
for (e in epu_list){
  message(e)
  for (i in 1:4){
    message(fname[i])
    assign(e,rbind(get(e),get_group_mean(fname = fname[i], epu_name = e, anom = TRUE)))
  }
}

#process output
seasonal_oisst_anom <- rbind(MAB,GOM,GB) %>%
    dplyr::mutate(Time = as.numeric(stringr::str_extract(year,"\\d{4}")),
           Var = paste(stringr::str_extract(year, "winter|spring|summer|fall"),"OI SST Anomaly")) %>%
    dplyr::select(-year) %>%
    dplyr::mutate(Units = "degreesC")


# metadata ----
attr(seasonal_oisst_anom, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/seasonal-sst-anomalies.html"
attr(seasonal_oisst_anom, "data_files")   <- list(
  seasonal_oisst_anom_nc = seasonal_oisst_anom_nc)
attr(seasonal_oisst_anom, "data_steward") <- c(
  "Kimberly Bastille <kimberly.bastille@noaa.gov>")

usethis::use_data(seasonal_oisst_anom, overwrite = TRUE)


