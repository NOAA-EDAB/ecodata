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

### SST from ecopull
# https://github.com/kimberly-bastille/ecopull/blob/main/data/new_sst_anomaly.rda
raw.dir <- here::here("data-raw")
sst_rda<- "new_sst_anomaly_2025.rda"

get_seasonal_oisst_anom<- function(save_clean = F){
  load(file.path(raw.dir, sst_rda))
  seasonal_oisst_anom <- new_sst_anomaly %>%
    dplyr::rename(Time = Year) %>%
    tidyr::separate(Var, into = c("X", "Var", "A", "b"), sep = "_") %>%
    dplyr::select(Time, Var, Value, EPU) %>%
    dplyr::mutate(Var = dplyr::recode(Var, "winter"="Winter",
                                      "spring" = "Spring",
                                      "summer" = "Summer",
                                      "fall" = "Fall"))

  if (save_clean){
    usethis::use_data(seasonal_oisst_anom, overwrite = T)
  } else {
    return(seasonal_oisst_anom)
  }
}
get_seasonal_oisst_anom(save_clean = T)




########### Old Methods ################
# raw.dir <- here::here("data-raw", "gridded", "sst")
# ltm.dir <- here::here("data-raw","gridded", "sst", "ltm")
#
# epu <- ecodata::epu_sf %>%
#   filter(EPU != "SS")
#
#
# seasonal_epu_ltm <- function(ltm, epu_name){
#   ltm <- mask(ltm, epu[epu$EPU == epu_name,])
#   ltm_out <- mean(ltm@data@values, na.rm = T)
#   return(ltm_out)
# }
#
# seasonal_oisst_anom_nc <-"sst.day.mean.ltm.1982-2010.nc"
#
# #Get long-term mean for anomaly calculation
# ltm <- raster::stack(file.path(ltm.dir,seasonal_oisst_anom_nc))
# ltm <- raster::crop(ltm, extent(280,300,30,50))
# #ltm <- raster::rotate(ltm)
#
# winter.ltm <- ltm[[1:90]]
# winter.ltm <- raster::stackApply(winter.ltm, indices = rep(1,raster::nlayers(winter.ltm)),mean)
#
# spring.ltm <- ltm[[91:181]]
# spring.ltm <- raster::stackApply(spring.ltm, indices = rep(1,raster::nlayers(spring.ltm)),mean)
#
# summer.ltm <- ltm[[182:273]]
# summer.ltm <- raster::stackApply(summer.ltm, indices = rep(1,raster::nlayers(summer.ltm)),mean)
#
# fall.ltm <- ltm[[274:365]]
# fall.ltm <- raster::stackApply(fall.ltm, indices = rep(1,raster::nlayers(fall.ltm)),mean)
#
#
# #Function to get seasonal averages by year
#
# get_group_mean <- function(fname, epu_name, anom = T){
#
#   #Import raster data
#   raw <- raster::stack(file.path(raw.dir, fname))
#
#   message("Cropping data")
#   crs(raw) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
#   raw <- raster::crop(raw, extent(280,300,30,50))
#
#   #Get layer index and map to year
#   message('Getting index')
#   year <- NULL
#   for (i in 1:raster::nlayers(raw)){
#     assign("year",rbind(year, data.frame(Time = str_extract(raw[[i]]@data@names,"\\d{4}"))))
#   }
#
#   year_split <- year %>%
#     dplyr::group_by(Time) %>%
#     dplyr::mutate(day = 1:n()) %>%
#     dplyr::mutate(leap = ifelse(n() == 365,"common","leap")) %>%
#     dplyr::group_by(leap) %>%
#     dplyr::mutate(season = ifelse(day <= 90 & leap == "common", "winter",
#                                ifelse(day > 90 & day <= 181 & leap == "common", "spring",
#                                       ifelse(day > 181 & day <= 273 & leap == "common", "summer",
#                                              ifelse(day > 273 & leap == "common", "fall",
#
#                         ifelse(day <= 91 & leap == "leap", "winter",
#                                ifelse(day > 91 & day <= 181 & leap == "leap", "spring",
#                                       ifelse(day > 181 & day <= 273 & leap == "leap", "summer",
#                                              ifelse(day > 273 & leap == "leap", "fall",NA))))))))) %>%
#     dplyr::group_by(Time, leap, season) %>%
#     dplyr::mutate(index = paste(Time, season))
#
#   if (any(is.na(year_split))){
#     message("NA in year")
#   }
#
#   #Rotate from 0-360 to -180-180
#   message(paste('Rotating',fname))
#   raw1 <- raster::rotate(raw)
#   #Split data on layer index - stackApply will break if there are too many layers
#   g1 <- year_split %>%
#     dplyr::filter(index %in% unique(.$index)[1:10]) %>%
#     pull(index)
#
#   g2 <- year_split %>%
#     dplyr::filter(!index %in% unique(.$index)[1:10]) %>%
#     pull(index)
#
#   #Apply and combine
#   message(paste('Finding mean'))
#   n <- raster::nlayers(raw1)
#   #rawMean1 <- raster::stackApply(raw1[[1:length(g1)]], indices = g1, fun =  mean)
#   #rawMean1 <- raster::stackApply(raw1[1:length(g1)], indices = g1,  mean)
#   rawMean1 <- raster::stackApply(raw1, indices = g1,  mean)
#   #rawMean2 <- raster::stackApply(raw1[[(length(g1) + 1):n]], indices = g2, mean)
#   #rawMean <- raster::stack(rawMean1,rawMean2)
#   rawMean <- raster::stack(rawMean1)
#
#   #Mask output to EPU
#   message(paste('Masking to',epu_name))
#   out <- raster::mask(rawMean, epu[epu$EPU == epu_name,])
#
#   #Find seasonal anomaly
#   mean_sst <- NULL
#   for (i in 1:raster::nlayers(out)){
#
#     if (anom){
#       season <- str_extract(names(out[[i]]),"winter|spring|summer|fall")
#       message(paste('Finding',season,'SST anomaly for',epu_name))
#       sst <- mean(out[[i]]@data@values, na.rm = T) - seasonal_epu_ltm(ltm = get(paste0(season,".ltm")),
#                                                                       epu = epu_name)
#       var <- "anomaly"
#     } else {
#       sst <- mean(out[[i]]@data@values, na.rm = T)
#       var <- "absolute"
#     }
#
#     year = out@data@names[i]
#     df <- data.frame(Value = sst,
#                      year = year,
#                      EPU = epu_name,
#                      Var = var)
#
#     assign('mean_sst',rbind(mean_sst, df))
#   }
#   return(mean_sst)
# }
#
# #get sst
# fname <- list.files(raw.dir, pattern = ".nc")
#
# MAB <- NULL
# GOM <- NULL
# GB <- NULL
#
#  epu_list <- c("MAB","GOM","GB")
# for (e in epu_list){
#   message(e)
#   for (i in 1:length(fname)){
#     message(fname[i])
#     assign(e,rbind(get(e),get_group_mean(fname = fname[i], epu_name = e, anom = TRUE)))
#   }
# }
#
# #process output
# seasonal_oisst_anom <- rbind(MAB,GOM,GB) %>%
#     dplyr::mutate(Time = as.numeric(stringr::str_extract(year,"\\d{4}")),
#            Var = paste(stringr::str_extract(year, "winter|spring|summer|fall"),"OI SST Anomaly")) %>%
#     dplyr::select(-year) %>%
#     dplyr::mutate(Units = "degreesC")







