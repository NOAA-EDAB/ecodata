## get_ESP_marine_heatwaves

## Get mean sst fro each stock area.

## Calculate Marine Heatwave Cumulative and Maximum
raw.dir<- here::here("data-raw","gridded","sst_data")
ltm.dir <- here::here("data-raw","gridded")

library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(ncdf4)
library(reshape2)
library(ecodata)
library(stringr)
library(tidync)

sst80<- raster::stack(file.path(here::here(raw.dir,"1982_1991_ne_sst.nc")))
sst80 <- raster::crop(sst80, extent(280,300,30,50))
sst80 <- raster::rotate(sst80)
sst90<- raster::stack(file.path(here::here(raw.dir,"1992_2001_ne_sst.nc")))
sst90 <- raster::crop(sst90, extent(280,300,30,50))
sst90 <- raster::rotate(sst90)
sst00<- raster::stack(file.path(here::here(raw.dir,"2002_2011_ne_sst.nc")))
sst00 <- raster::crop(sst00, extent(280,300,30,50))
sst00 <- raster::rotate(sst00)
sst10<- raster::stack(file.path(here::here(raw.dir,"2012_2020_ne_sst.nc")))
sst10 <- raster::crop(sst10, extent(280,300,30,50))
sst10 <- raster::rotate(sst10)

sst<- raster::stack(sst80, sst90, sst00, sst10)

sf <- ecodata::ESP_sf

crs(sst) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

get_stock_heatwave<- function(stock){
  crop<- mask(sst, sf[sf$ID == stock,])
  value<- raster::cellStats(crop, stat='mean', na.rm=TRUE)
  dat<- data.frame(value) %>%
    dplyr::mutate(temp = as.numeric(value)) %>%
    tibble::rownames_to_column("t") %>%
    dplyr::mutate(t = substr(t, 2, 11),
                  t = lubridate::as_date(t)) %>%
    dplyr::select(t, temp)
  dat<-as.tibble(dat)
  ts <- heatwaveR::ts2clm(dat, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  stock.mhw <- heatwaveR::detect_event(ts)
  mhw<- stock.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative)%>%
    dplyr::mutate(stock_id = stock)# add max year ## days in 2020 data set only went to dec 9, 2020

  return(mhw)
}

acadian_redfish_both<- get_stock_heatwave("acadian_redfish_both")
alewife_both<- get_stock_heatwave("alewife_both")
american_lobster_gb_both<-  get_stock_heatwave("american_lobster_gb_both")
american_lobster_gom_both<-  get_stock_heatwave("american_lobster_gom_both")
american_lobster_sne_both<-  get_stock_heatwave("american_lobster_sne_both")
american_plaice_both<-  get_stock_heatwave("american_plaice_both")
atlantic_cod_gb_both<-  get_stock_heatwave("atlantic_cod_gb_both")
atlantic_hagfish_both<-  get_stock_heatwave("atlantic_hagfish_both")
atlantic_halibut_both<-  get_stock_heatwave("atlantic_halibut_both")
atlantic_herring_spring<-  get_stock_heatwave("atlantic_herring_spring")
atlantic_herring_fall<-  get_stock_heatwave("atlantic_herring_fall")
atlantic_menhaden_fall<-  get_stock_heatwave("atlantic_menhaden_fall")
atlantic_mackerel_spring<-  get_stock_heatwave("atlantic_mackerel_spring")
atlantic_menhaden_north_spring<-  get_stock_heatwave("atlantic_menhaden_north_spring")
atlantic_menhaden_north_fall<-  get_stock_heatwave("atlantic_menhaden_north_fall")
atlantic_menhaden_south_fall<-  get_stock_heatwave("atlantic_menhaden_south_fall")
atlantic_menhaden_south_spring<-  get_stock_heatwave("atlantic_menhaden_south_spring")
atlantic_menhaden_spring<-  get_stock_heatwave("atlantic_menhaden_spring")
atlantic_wolffish_both<-  get_stock_heatwave("atlantic_wolffish_both")
barndoor_skate_both<-  get_stock_heatwave("barndoor_skate_both")
black_sea_bass_north_spring <-  get_stock_heatwave("black_sea_bass_north_spring")
black_sea_bass_spring <- get_stock_heatwave("black_sea_bass_spring")
black_sea_bass_south_spring <- get_stock_heatwave("black_sea_bass_south_spring")
blueback_herring_both<- get_stock_heatwave("blueback_herring_both")
bluefish_fall<- get_stock_heatwave("bluefish_fall")
butterfish_both<- get_stock_heatwave("butterfish_both")
clearnose_skate_both<- get_stock_heatwave("clearnose_skate_both")
cusk_both<- get_stock_heatwave("cusk_both")
haddock_gb_both<- get_stock_heatwave("haddock_gb_both")
little_skate_both<- get_stock_heatwave("little_skate_both")
longfin_inshore_squid_both<- get_stock_heatwave("longfin_inshore_squid_both")
monkfish_north_both<- get_stock_heatwave("monkfish_north_both")
monkfish_south_both<- get_stock_heatwave("monkfish_south_both")
northern_shortfin_squid_both<- get_stock_heatwave("northern_shortfin_squid_both")
northern_shrimp_fall<- get_stock_heatwave("northern_shrimp_fall")
ocean_pout_spring<- get_stock_heatwave("ocean_pout_spring")
offshore_hake_both<- get_stock_heatwave("offshore_hake_both")
red_hake_gom_ngb_both<- get_stock_heatwave("red_hake_gom_ngb_both")
red_hake_sgb_ma_both<- get_stock_heatwave("red_hake_sgb_ma_both")
rosette_skate_both<- get_stock_heatwave("rosette_skate_both")
scup_fall<- get_stock_heatwave("scup_fall")
scup_spring<- get_stock_heatwave("scup_spring")
silver_hake_gom_ngb_both<- get_stock_heatwave("silver_hake_gom_ngb_both")
silver_hake_sgb_ma_both<- get_stock_heatwave("silver_hake_sgb_ma_both")
smooth_dogfish_fall<- get_stock_heatwave("smooth_dogfish_fall")
smooth_skate_both<- get_stock_heatwave("smooth_skate_both")
spiny_dogfish_both<- get_stock_heatwave("spiny_dogfish_both")
summer_flounder_fall<- get_stock_heatwave("summer_flounder_fall")
summer_flounder_spring<- get_stock_heatwave("summer_flounder_spring")
thorny_skate_both<- get_stock_heatwave("thorny_skate_both")
windowpane_flounder_gom_gb_both<- get_stock_heatwave("windowpane_flounder_gom_gb_both")
windowpane_flounder_sne_ma_both<- get_stock_heatwave("windowpane_flounder_sne_ma_both")
winter_flounder_gb_both<- get_stock_heatwave("winter_flounder_gb_both")
winter_flounder_gom_both<- get_stock_heatwave("winter_flounder_gom_both")
winter_flounder_sne_ma_both<- get_stock_heatwave("winter_flounder_sne_ma_both")
winter_skate_both<- get_stock_heatwave("winter_skate_both")
witch_flounder_both<- get_stock_heatwave("witch_flounder_both")
white_hake_both<- get_stock_heatwave("white_hake_both")
yellowtail_flounder_cc_spring<- get_stock_heatwave("yellowtail_flounder_cc_spring")
yellowtail_flounder_cc_fall<- get_stock_heatwave("yellowtail_flounder_cc_fall")
yellowtail_flounder_gb_both<- get_stock_heatwave("yellowtail_flounder_gb_both")
yellowtail_flounder_sne_ma_spring<- get_stock_heatwave("yellowtail_flounder_sne_ma_spring")
yellowtail_flounder_sne_ma_fall<- get_stock_heatwave("yellowtail_flounder_sne_ma_fall")


# Cumulative intensity
cum.intensity <- rbind(acadian_redfish_both,
                       alewife_both,
                       american_lobster_gb_both,
                       american_lobster_gom_both,
                       american_lobster_sne_both,
                       american_plaice_both,
                       atlantic_cod_gb_both,
                       atlantic_hagfish_both,
                       atlantic_halibut_both ,
                       atlantic_herring_spring,
                       atlantic_herring_fall,
                       atlantic_menhaden_fall,
                       atlantic_mackerel_spring,
                       atlantic_menhaden_north_spring,
                       atlantic_menhaden_north_fall,
                       atlantic_menhaden_south_fall,
                       atlantic_menhaden_south_spring,
                       atlantic_menhaden_spring,
                       atlantic_wolffish_both,
                       barndoor_skate_both,
                       black_sea_bass_north_spring ,
                       black_sea_bass_spring ,
                       black_sea_bass_south_spring ,
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
  dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y"))) %>%
  dplyr::group_by(Time, stock_id) %>%
  dplyr::summarise(Value = as.numeric(sum(intensity_cumulative))) %>%
  dplyr::mutate(Var = "cumulative intensity") %>%
  dplyr::ungroup()
#Max intensity
max.intensity <- rbind(acadian_redfish_both,
                       alewife_both,
                       american_lobster_gb_both,
                       american_lobster_gom_both,
                       american_lobster_sne_both,
                       american_plaice_both,
                       atlantic_cod_gb_both,
                       atlantic_hagfish_both,
                       atlantic_halibut_both ,
                       atlantic_herring_spring,
                       atlantic_herring_fall,
                       atlantic_menhaden_fall,
                       atlantic_mackerel_spring,
                       atlantic_menhaden_north_spring,
                       atlantic_menhaden_north_fall,
                       atlantic_menhaden_south_fall,
                       atlantic_menhaden_south_spring,
                       atlantic_menhaden_spring,
                       atlantic_wolffish_both,
                       barndoor_skate_both,
                       black_sea_bass_north_spring ,
                       black_sea_bass_spring ,
                       black_sea_bass_south_spring ,
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
  dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")))  %>%
  dplyr::rename(Value = intensity_max) %>%
  dplyr::mutate(Var = "maximum intensity")%>%
  dplyr::select(Time, stock_id, Value, Var)


ESP_heatwave<- rbind(cum.intensity, max.intensity) %>%
  dplyr:: mutate(Units = "degrees C",
                 Time = as.numeric(Time))


usethis::use_data(ESP_heatwave, overwrite = T)



########################### Cod Stock Areas #############################
raw.dir<- here::here("data-raw","gridded","sst")
ltm.dir <- here::here("data-raw","gridded")

library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(ncdf4)
library(reshape2)
library(ecodata)
library(stringr)
library(tidync)

sst80<- raster::stack(file.path(here::here(raw.dir,"1982_1991_ne_sst.nc")))
sst80 <- raster::crop(sst80, extent(280,300,30,50))
sst80 <- raster::rotate(sst80)
sst90<- raster::stack(file.path(here::here(raw.dir,"1992_2001_ne_sst.nc")))
sst90 <- raster::crop(sst90, extent(280,300,30,50))
sst90 <- raster::rotate(sst90)
sst00<- raster::stack(file.path(here::here(raw.dir,"2002_2011_ne_sst.nc")))
sst00 <- raster::crop(sst00, extent(280,300,30,50))
sst00 <- raster::rotate(sst00)
sst12<- raster::stack(file.path(here::here(raw.dir,"sst.day.mean.2012.v2.nc")))
sst12 <- raster::crop(sst12, extent(280,300,30,50))
sst12 <- raster::rotate(sst12)
sst13<- raster::stack(file.path(here::here(raw.dir,"sst.day.mean.2013.v2.nc")))
sst13 <- raster::crop(sst13, extent(280,300,30,50))
sst13 <- raster::rotate(sst13)
sst14<- raster::stack(file.path(here::here(raw.dir,"sst.day.mean.2014.v2.nc")))
sst14 <- raster::crop(sst14, extent(280,300,30,50))
sst14 <- raster::rotate(sst14)
sst15<- raster::stack(file.path(here::here(raw.dir,"sst.day.mean.2015.v2.nc")))
sst15 <- raster::crop(sst15, extent(280,300,30,50))
sst15 <- raster::rotate(sst15)
sst16<- raster::stack(file.path(here::here(raw.dir,"sst.day.mean.2016.v2.nc")))
sst16 <- raster::crop(sst16, extent(280,300,30,50))
sst16 <- raster::rotate(sst16)
sst17<- raster::stack(file.path(here::here(raw.dir,"sst.day.mean.2017.v2.nc")))
sst17 <- raster::crop(sst17, extent(280,300,30,50))
sst17 <- raster::rotate(sst17)
sst18<- raster::stack(file.path(here::here(raw.dir,"sst.day.mean.2018.v2.nc")))
sst18 <- raster::crop(sst18, extent(280,300,30,50))
sst18 <- raster::rotate(sst18)
sst19<- raster::stack(file.path(here::here(raw.dir,"sst.day.mean.2019.v2.nc")))
sst19 <- raster::crop(sst19, extent(280,300,30,50))
sst19 <- raster::rotate(sst19)
sst20<- raster::stack(file.path(here::here(raw.dir,"sst.day.mean.2020.v2.nc")))
sst20 <- raster::crop(sst20, extent(280,300,30,50))
sst20 <- raster::rotate(sst20)
sst21<- raster::stack(file.path(here::here(raw.dir,"sst.day.mean.2021.nc")))
sst21 <- raster::crop(sst21, extent(280,300,30,50))
sst21 <- raster::rotate(sst21)

sst<- raster::stack(sst80, sst90, sst00, sst12, sst13,
                    sst14, sst15, sst16, sst17, sst18,
                    sst19, sst20, sst21)
crs(sst) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

sf_cod<-rgdal::readOGR(file.path(here::here("data-raw", "ESP"), "codstox.shp"), verbose = F)
crs(sf_cod) <- crs
sf <- as(sf_cod, "sf") %>%
  dplyr::mutate(ID = STOCK)

get_stock_heatwave<- function(stock){
  crop<- mask(sst, sf[sf$ID == stock,])
  value<- raster::cellStats(crop, stat='mean', na.rm=TRUE)
  dat<- data.frame(value) %>%
    dplyr::mutate(temp = as.numeric(value)) %>%
    tibble::rownames_to_column("t") %>%
    dplyr::mutate(t = substr(t, 2, 11),
                  t = lubridate::as_date(t)) %>%
    dplyr::select(t, temp)
  dat<-as.tibble(dat)
  ts <- heatwaveR::ts2clm(dat, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  stock.mhw <- heatwaveR::detect_event(ts)
  mhw<- stock.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative)%>%
    dplyr::mutate(stock_id = stock)# add max year ## days in 2020 data set only went to dec 9, 2020

  return(mhw)
}

#acadian_redfish_both<- get_stock_heatwave("acadian_redfish_both")
egom <- get_stock_heatwave("EGOM")
gbk<- get_stock_heatwave("GBK")
wgom<- get_stock_heatwave("WGOM")
sne<- get_stock_heatwave("SNE")

cum.intensity <- rbind(egom, gbk, wgom, sne) %>%
  dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y"))) %>%
  dplyr::group_by(Time, stock_id) %>%
  dplyr::summarise(Value = as.numeric(sum(intensity_cumulative))) %>%
  dplyr::mutate(Var = "cumulative intensity") %>%
  dplyr::ungroup()
#Max intensity
max.intensity <- rbind(egom, gbk, wgom, sne) %>%
  dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")))  %>%
  dplyr::rename(Value = intensity_max) %>%
  dplyr::mutate(Var = "maximum intensity")%>%
  dplyr::select(Time, stock_id, Value, Var)


ESP_heatwave_cod<- rbind(cum.intensity, max.intensity) %>%
  dplyr:: mutate(Units = "degrees C",
                 Time = as.numeric(Time))

usethis::use_data(ESP_heatwave_cod, overwrite = T)



###### testting

sst10<- raster::stack(file.path(here::here(raw.dir,"sst.day.mean.2012.v2.nc")),
                      file.path(here::here(raw.dir,"sst.day.mean.2013.v2.nc")),
                      file.path(here::here(raw.dir,"sst.day.mean.2014.v2.nc")),
                      file.path(here::here(raw.dir,"sst.day.mean.2015.v2.nc")),
                      file.path(here::here(raw.dir,"sst.day.mean.2016.v2.nc")),
                      file.path(here::here(raw.dir,"sst.day.mean.2017.v2.nc")),
                      file.path(here::here(raw.dir,"sst.day.mean.2018.v2.nc")),
                      file.path(here::here(raw.dir,"sst.day.mean.2019.v2.nc")),
                      file.path(here::here(raw.dir,"sst.day.mean.2020.v2.nc")),
                      file.path(here::here(raw.dir,"sst.day.mean.2021.nc")))

