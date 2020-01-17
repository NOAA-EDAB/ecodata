# A general function to process various OI data sets

library(dplyr)
library(tidyr)
library(lubridate)
library(raster)
library(rgdal)
library(sf)

process_oi <- function(variable, type = NULL, season, genus = NULL, epu){

  if(!is.null(type) & !variable %in% c("salinity","temperature")){
    stop('type only applicable for variables "salinity" and "temperature"
         as type = "bottom" or type = "surface"')
  }

  if(!is.null(genus) & variable != "zooplankton"){
    stop('genus only applicable for variable "zooplankton"')
  }

  #get compiled down-sampled raster of chosen strata from shapefile.
  epumask.raster <- get(paste0(tolower(epu),"_rast"))

  #get bottom temp data and find mean for stock area--------------------------------------

  indir <- here::here("data-raw","gridded")

  if(variable == "zooplankton"){
    load(file.path(indir, paste0(genus,"_",season,"_zoo_1977-2017.rdata")))
  }

  # if (variable == "salinity"){
  #   load(file.path(indir, paste0("sal_",type,"_",season,"_spdf.rdata")))
  # } else if (variable == "temperature"){
  #   load(file.path(indir, paste0("temp_",type,"_",season,"_spdf.rdata")))
  # } else if (variable == "chlorophyll"){
  #   load(file.path(indir, paste0("chl_",season,"_1997-2018.rdata")))
  # } else if (variable == "zooplankton"){
  #   load(file.path(indir, paste0(genus,"_",season,"_zoo_1977-2017.rdata")))
  # }


  #create null df to fill with results
  data = data.frame(array(NA,dim= c(raster::nlayers(ecsa_dat),5)))
  #data = data.frame(array(NA,dim= c(raster::nlayers(str_detect(ecsa_dat),5))))

  #loops through layers in raster brick
  for(i in 1:raster::nlayers(ecsa_dat)){
    #load raster by year
    print(i)
    #get file information from title
    #layer_id <- stringr::str_extract(names(ecsa_dat)[[i]], "\\d.*")
    layer_id <- stringr::str_split(names(ecsa_dat)[[i]], "_")
    data[i,1] <- layer_id[[1]][[1]]
    data[i,2] <- layer_id[[1]][[2]]
    data[i,3] <- layer_id[[1]][[3]]

    #trim to stock area
    masked.raster = ecsa_dat[[i]]*epumask.raster

    #find mean BT of stock area
    data[i,4] = raster::cellStats(masked.raster, stat='mean', na.rm=TRUE)
    data[i,5] = raster::cellStats(masked.raster, stat = 'sd', na.rm=TRUE)
    #
    # if (layer_id[[1]][[1]] == "1995"){
    #   break
    # }
  }
  x <- as.numeric(data$X1)
  y.out <- data$X4
  y.sd <- data$X5

  sd.low <- y.out - y.sd
  sd.high <- y.out + y.sd

  # remove
  if (variable == "zooplankton"){
    if (season == "Spring"){
      y.out[x %in% c(1989, 1990, 1991, 1994)] <- NA
      sd.low[x %in% c(1989, 1990, 1991, 1994)] <- NA
      sd.high[x %in% c(1989, 1990, 1991, 1994)] <- NA
    } else if (season == "Fall") {
      y.out[x %in% c(1989, 1990, 1992)] <- NA
      sd.low[x %in% c(1989, 1990, 1992)] <- NA
      sd.high[x %in% c(1989, 1990, 1992)] <- NA
    }
  }

  if(variable == "chlorophyll"){
    type <- ""
  } else if (variable == "zooplankton"){
    type <- genus
    variable <- "zoo"
  }

  out <- data.frame(Var = paste(paste(type,variable),season),
                    Time = as.numeric(x),
                    Value = y.out,
                    sd.low = sd.low,
                    sd.high = sd.high,
                    Season = season,
                    epu = epu)

  out <- out[out$Time > 1968,]
  return(out)

}
