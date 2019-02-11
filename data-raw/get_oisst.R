#Analysis of OISST V2 data to extract seasonal SST time series 

library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(ncdf4)
library(reshape2)
library(ecodata)
library(stringr)

raw.dir <- here::here("inst","extdata","gridded","sst_data")


epu <- ecodata::epu_sf %>% 
  filter(EPU != "SS")

get_group_mean <- function(fname, epu_name){
  raw <- stack(file.path(raw.dir, fname))
  
  crs(raw) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

  message('Getting index')
  year <- NULL
  for (i in 1:nlayers(raw)){
    assign("year",rbind(year, data.frame(Time = str_extract(raw[[i]]@data@names,"\\d{4}"))))
  }
  
  year_split <- year %>% 
    group_by(Time) %>%
      mutate(day = 1:n()) %>% 
      mutate(leap = ifelse(n() == 365,"common","leap")) %>% 
      group_by(leap) %>% 
        mutate(season = ifelse(day <= 90 & leap == "common", "winter",
                               ifelse(day > 90 & day <= 181 & leap == "common", "spring",
                                      ifelse(day > 181 & day <= 273 & leap == "common", "summer",
                                             ifelse(day > 273 & leap == "common", "fall",
                                                    
                        ifelse(day <= 91 & leap == "leap", "winter",
                               ifelse(day > 91 & day <= 181 & leap == "leap", "spring",
                                      ifelse(day > 181 & day <= 273 & leap == "leap", "summer",
                                             ifelse(day > 273 & leap == "leap", "fall",NA))))))))) %>% 
    group_by(Time, leap, season) %>% 
    mutate(index = paste(Time, season)) %>% 
    pull(index)
  
  if (any(is.na(year_split))){
    message("NA in year")
  }
  
  message(paste('Rotating',fname))
  raw1 <- rotate(raw)
  
  message(paste('Finding mean'))
  
  n <- nlayers(raw1)
  g1 <- floor(n/2)
  rawMean1 <- stackApply(raw1[[1:g1]], indices = year_split[1:g1], mean)
  rawMean2 <- stackApply(raw1[[(g1+1):n]], indices = year_split[(g1+1):n], mean)
  rawMean <- stack(rawMean1,rawMean2)
  
  message(paste('Masking to',epu_name))
  out <- mask(rawMean, epu[epu$EPU == epu_name,])
  
  mean_sst <- NULL
  for (i in 1:nlayers(out)){
    sst = mean(out[[i]]@data@values, na.rm = T)
    year = out@data@names[i]
    
    df <- data.frame(sst = sst,
                     year = year,
                     epu = epu_name)
    
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
    assign(e,rbind(get(e),get_group_mean(fname = fname[i], epu_name = e)))
  }
}

seasonal_oisst <- rbind(MAB,GOM,GB) %>% 
    mutate(Time = str_extract(year,"\\d{4}"),
           Var = paste(str_extract(year, "winter|spring|summer|fall"),"OI SST")) %>% 
    dplyr::select(-year,
                  Value = sst,
                  EPU = epu) %>% 
    mutate(Units = "degreesC")

usethis::use_data(seasonal_oisst, overwrite = TRUE)
  

