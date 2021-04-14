# ESP chlorophyll

## data comes from Kim Hyde and "season"_chl.nc were constructed in spatial warnings
## Using the code at this link https://github.com/kimberly-bastille/sp-warnings/blob/master/R/get_seasonal_chl_pp.R

## The output from this code was used as raw data in this script.


library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(ncdf4)
library(reshape2)
library(ecodata)
library(stringr)

raw.dir <- here::here("data-raw","ESP")

ESP_sf <- ecodata::ESP_sf

# season <- "winter"
# ESP_name <- "acadian_redfish_both"


get_group_mean <- function(season, ESP_name, anom = T){

  #Import raster data
  season.pp <- raster::stack(file.path(raw.dir, paste0(season,"_pp.nc")))

  #crs(raw) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

  # Mask to ESP
  message(paste('Masking to',ESP_name))
  out <- raster::mask(season.pp, ESP_sf[ESP_sf$ID == ESP_name,])

  #Find seasonal anomaly
  mean_pp <- NULL
  for (k in 1:nlayers(out)){

    pp <- mean(out[[k]]@data@values, na.rm = T)
    # var <- season
    # year = out@data@names[i]
    df <- data.frame(Value = pp,
                     year = 1997 + k,
                     ESP = ESP_name,
                     Var = season)
    mean_pp<- df %>% rbind(mean_pp)

    #assign('ESP_chl',rbind(mean_chl, df))
  }
  return(mean_pp)
}

season_list<- c("winter", "spring",
                "summer", "fall")


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
black_sea_bass_north_spring <- NULL
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
  for (i in season_list){
    assign(e,rbind(get(e),get_group_mean(season = i, ESP_name = e, anom = TRUE)))
  }
}


#process output
ESP_seasonal_pp <- rbind(acadian_redfish_both,
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
                          black_sea_bass_north_spring,
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
  #dplyr::mutate(#Time = as.numeric(stringr::str_extract(year,"\\d{4}")),
  #Var = paste(stringr::str_extract(year, "winter|spring|summer|fall"),"OI SST Anomaly")) %>%
  #dplyr::select(-year) %>%
  dplyr::mutate(Units = "gC m−2 d−1") %>%
  dplyr::rename(Time = year)


# metadata ----
#attr(ESP_seasonal_chl, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/seasonal-sst-anomalies.html"
# attr(ESP_seasonal_chl, "data_files")   <- list(
#   seasonal_oisst_anom_nc = seasonal_oisst_anom_nc)
#attr(ESP_seasonal_chl, "data_steward") <- c(
#  "Kimberly Bastille <kimberly.bastille@noaa.gov>")

usethis::use_data(ESP_seasonal_pp, overwrite = TRUE)



















