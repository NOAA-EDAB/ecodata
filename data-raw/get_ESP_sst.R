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

ESP_sf <- ecodata::ESP_sf


seasonal_ESP_ltm <- function(ltm, ESP_name){
  ltm <- mask(ltm, ESP_sf[ESP_sf$ID == ESP_name,])
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

get_group_mean <- function(fname, ESP_name, anom = T){

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
  message(paste('Masking to',ESP_name))
  out <- raster::mask(rawMean, ESP_sf[ESP_sf$ID == ESP_name,])

  #Find seasonal anomaly
  mean_sst <- NULL
  for (i in 1:nlayers(out)){

    if (anom){
      season <- str_extract(names(out[[i]]),"winter|spring|summer|fall")
      message(paste('Finding',season,'SST anomaly for',ESP_name))
      sst <- mean(out[[i]]@data@values, na.rm = T) - seasonal_ESP_ltm(ltm = get(paste0(season,".ltm")),
                                                                      ESP = ESP_name)
      var <- "anomaly"
    } else {
      sst <- mean(out[[i]]@data@values, na.rm = T)
      var <- "absolute"
    }

    year = out@data@names[i]
    df <- data.frame(Value = sst,
                     year = year,
                     ESP = ESP_name,
                     Var = var)

    assign('mean_sst',rbind(mean_sst, df))
  }
  return(mean_sst)
}

#get sst
fname <- list.files(raw.dir)

### Changes start here
acadian_redfish_both<- NULL
alewife_both<- NULL
american_lobster_gb_both<- NULL
american_lobster_gom_both<- NULL
american_lobster_sne_both<- NULL
american_plaice_both<- NULL
atlantic_cod_gb_both<- NULL
atlantic_hagfish_both<- NULL
atlantic_halibut_both<- NULL
atlantic_herring_spring<- NULL
atlantic_herring_fall<- NULL
atlantic_menhaden_fall<- NULL
atlantic_mackerel_spring<- NULL
atlantic_menhaden_north_spring<- NULL
atlantic_menhaden_north_fall<- NULL
atlantic_menhaden_south_fall<- NULL
atlantic_menhaden_south_spring<- NULL
atlantic_menhaden_spring<- NULL
atlantic_wolffish_both<- NULL
barndoor_skate_both<- NULL
black_sea_bass_north_pring <- NULL
black_sea_bass_spring <- NULL
black_sea_bass_south_spring <- NULL
blueback_herring_both<- NULL
bluefish_fall<- NULL
butterfish_both<- NULL
clearnose_skate_both<- NULL
cusk_both<- NULL
haddock_gb_both<- NULL
little_skate_both<- NULL
longfin_inshore_squid_both<- NULL
monkfish_north_both<- NULL
monkfish_south_both<- NULL
northern_shortfin_squid_both<- NULL
northern_shrimp_fall<- NULL
ocean_pout_spring<- NULL
offshore_hake_both<- NULL
red_hake_gom_ngb_both<- NULL
red_hake_sgb_ma_both<- NULL
rosette_skate_both<- NULL
scup_fall<- NULL
scup_spring<- NULL
silver_hake_gom_ngb_both<- NULL
silver_hake_sgb_ma_both<- NULL
smooth_dogfish_fall<- NULL
smooth_skate_both<- NULL
spiny_dogfish_both<- NULL
summer_flounder_fall<- NULL
summer_flounder_spring<- NULL
thorny_skate_both<- NULL
windowpane_flounder_gom_gb_both<- NULL
windowpane_flounder_sne_ma_both<- NULL
winter_flounder_gb_both<- NULL
winter_flounder_gom_both<- NULL
winter_flounder_sne_ma_both<- NULL
winter_skate_both<- NULL
witch_flounder_both<- NULL
white_hake_both<- NULL
yellowtail_flounder_cc_spring<- NULL
yellowtail_flounder_cc_fall<- NULL
yellowtail_flounder_gb_both<- NULL
yellowtail_flounder_sne_ma_spring<- NULL
yellowtail_flounder_sne_ma_fall<- NULL

stock_list2 <- c("alewife_both","blueback_herring_both","atlantic_menhaden_north_fall",
                 "atlantic_menhaden_fall","atlantic_hagfish_both","spiny_dogfish_both",
                 "barndoor_skate_both","winter_skate_both","little_skate_both",
                 "smooth_skate_both","thorny_skate_both","atlantic_herring_spring",
                 "atlantic_herring_fall","silver_hake_gom_ngb_both","white_hake_both",
                 "red_hake_gom_ngb_both","atlantic_halibut_both","american_plaice_both",
                 "yellowtail_flounder_cc_spring","yellowtail_flounder_cc_fall","winter_flounder_gom_both",
                 "witch_flounder_both","atlantic_mackerel_spring","acadian_redfish_both",
                 "atlantic_wolffish_both","monkfish_north_both","american_lobster_gom_both",
                 "northern_shrimp_fall","northern_shortfin_squid_both","windowpane_flounder_gom_gb_both",
                 "cusk_both","ocean_pout_spring","longfin_inshore_squid_both",
                 "butterfish_both","haddock_gb_both","smooth_dogfish_fall",
                 "atlantic_cod_gb_both","american_lobster_gb_both","winter_flounder_gb_both",
                 "yellowtail_flounder_gb_both","summer_flounder_fall","scup_fall",
                 "winter_flounder_sne_ma_both","atlantic_menhaden_north_spring","atlantic_menhaden_spring",
                 "offshore_hake_both","silver_hake_sgb_ma_both","red_hake_sgb_ma_both",
                 "monkfish_south_both" ,"scup_spring","black_sea_bass_north_spring",
                 "black_sea_bass_spring","american_lobster_sne_both", "bluefish_fall",
                 "windowpane_flounder_sne_ma_both","summer_flounder_spring", "yellowtail_flounder_sne_ma_spring",
                 "yellowtail_flounder_sne_ma_fall","clearnose_skate_both","rosette_skate_both",
                 "black_sea_bass_south_spring","atlantic_menhaden_south_fall","atlantic_menhaden_south_spring" )
for (e in stock_list2){
  message(e)
  for (i in 1:4){
    message(fname[i])
    assign(e,rbind(get(e),get_group_mean(fname = fname[i], ESP_name = e, anom = TRUE)))
  }
}

#process output
ESP_seasonal_oisst_anom <- rbind(acadian_redfish_both,
                                 alewife_both,
                                 american_lobster_gb_both,
                                 american_lobster_gom_both,
                                 american_lobster_sne_both,
                                 american_plaice_both,
                                 atlantic_cod_gb_both,
                                 atlantic_hagfish_both,
                                 atlantic_halibut_both,
                                 atlantic_herring_spring,
                                 atlantic_herring_fall,
                                 atlantic_mackerel_spring,
                                 atlantic_menhaden_north_spring,
                                 atlantic_menhaden_north_fall,
                                 atlantic_menhaden_south_fall,
                                 atlantic_menhaden_south_spring,
                                 atlantic_menhaden_spring,
                                 atlantic_wolffish_both,
                                 barndoor_skate_both,
                                 black_sea_bass_north_pring,
                                 black_sea_bass_spring,
                                 black_sea_bass_south_spring,
                                 blueback_herring_both,
                                 bluefish_fall,
                                 butterfish_both,
                                 clearnose_skate_both,
                                 cusk_both,
                                 haddock_gb_both,
                                 little_skate_both,
                                 longfin_inshore_squid_both,
                                 monkfish_north_both,
                                 monkfish_south_both,
                                 northern_shortfin_squid_both,
                                 northern_shrimp_fall,
                                 ocean_pout_spring,
                                 offshore_hake_both,
                                 red_hake_gom_ngb_both,
                                 red_hake_sgb_ma_both,
                                 rosette_skate_both,
                                 scup_fall,
                                 scup_spring,
                                 silver_hake_gom_ngb_both,
                                 silver_hake_sgb_ma_both,
                                 smooth_dogfish_fall,
                                 smooth_skate_both,
                                 spiny_dogfish_both,
                                 summer_flounder_fall,
                                 summer_flounder_spring,
                                 thorny_skate_both,
                                 windowpane_flounder_gom_gb_both,
                                 windowpane_flounder_sne_ma_both,
                                 winter_flounder_gb_both,
                                 winter_flounder_gom_both,
                                 winter_flounder_sne_ma_both,
                                 winter_skate_both,
                                 witch_flounder_both,
                                 white_hake_both,
                                 yellowtail_flounder_cc_spring,
                                 yellowtail_flounder_cc_fall,
                                 yellowtail_flounder_gb_both,
                                 yellowtail_flounder_sne_ma_spring,
                                 yellowtail_flounder_sne_ma_fall) %>%
  dplyr::mutate(Time = as.numeric(stringr::str_extract(year,"\\d{4}")),
                Var = paste(stringr::str_extract(year, "winter|spring|summer|fall"),"OI SST Anomaly")) %>%
  dplyr::select(-year) %>%
  dplyr::mutate(Units = "degreesC")


# metadata ----
attr(ESP_seasonal_oisst_anom, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/seasonal-sst-anomalies.html"
attr(ESP_seasonal_oisst_anom, "data_files")   <- list(
  seasonal_oisst_anom_nc = seasonal_oisst_anom_nc)
attr(ESP_seasonal_oisst_anom, "data_steward") <- c(
  "Kimberly Bastille <kimberly.bastille@noaa.gov>")

usethis::use_data(ESP_seasonal_oisst_anom, overwrite = TRUE)


