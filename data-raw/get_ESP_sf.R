library(sf)
library(tidyverse)


get_ESP_sf <- function(save_clean = F){
## Stock list csv
stock_list<- read.csv(file.path(here::here("data-raw/ESP/stock_list.csv"))) %>%
  dplyr::mutate(STRATA = strata) # change to caps to left_join in next step.


### BTS shapefiles joined with stock to create df of all stock areas for each stock by area and season.
strata_int <- sf::st_read(here::here("data-raw/ESP/BTS_Strata.shp"),
                          quiet = TRUE) %>%
  left_join(stock_list, by = "STRATA") %>%
  dplyr::select("STRATA", "stock_name", "stock_season", "geometry") %>%
  tidyr::unite(stock, stock_name:stock_season, sep = "-")

## List of stocks with area and season
stock_list2 <- c("alewife-both","blueback-herring-both","atlantic-menhaden_north-fall",
                 "atlantic-menhaden-fall","atlantic-hagfish-both","spiny-dogfish-both",
                 "barndoor-skate-both","winter-skate-both","little-skate-both",
                 "smooth-skate-both","thorny-skate-both","atlantic-herring-spring",
                 "atlantic-herring-fall","silver-hake_gom-ngb-both","white-hake-both",
                 "red-hake_gom-ngb-both","atlantic-halibut-both","american-plaice-both",
                 "yellowtail-flounder_cc-spring","yellowtail-flounder_cc-fall","winter-flounder_gom-both",
                 "witch-flounder-both","atlantic-mackerel-spring","acadian-redfish-both",
                 "atlantic-wolffish-both","monkfish_north-both","american-lobster_gom-both",
                 "northern-shrimp-fall","northern-shortfin-squid-both","windowpane-flounder_gom-gb-both",
                 "cusk-both","ocean-pout-spring","longfin-inshore-squid-both",
                 "butterfish-both","haddock_gb-both","smooth-dogfish-fall",
                 "atlantic-cod_gb-both","american-lobster_gb-both","winter-flounder_gb-both",
                 "yellowtail-flounder_gb-both","summer-flounder-fall","scup-fall",
                 "winter-flounder_sne-ma-both","atlantic-menhaden_north-spring","atlantic-menhaden-spring",
                 "offshore-hake-both","silver-hake_sgb-ma-both","red-hake_sgb-ma-both",
                 "monkfish_south-both" ,"scup-spring","black-sea-bass_north-spring",
                 "black-sea-bass-spring","american-lobster_sne-both", "bluefish-fall",
                 "windowpane-flounder_sne-ma-both","summer-flounder-spring", "yellowtail-flounder_sne-ma-spring",
                 "yellowtail-flounder_sne-ma-fall","clearnose-skate-both","rosette-skate-both",
                 "black-sea-bass_south-spring","atlantic-menhaden_south-fall","atlantic-menhaden_south-spring" )


  #i = "black-sea-bass-spring"

empty <- data.frame(geometry = character(), ID = character())
## create single polygon for each stack area and season
for(i in stock_list2){
  test<- strata_int %>% dplyr::filter(stock == i) %>%
    sf::st_union()

  layer<-as.data.frame(test) %>%
    dplyr::mutate(ID = i)

  final<- rbind(empty, layer, final)

}


ESP_sf <- sf::st_as_sf(final)

if (save_clean){
  usethis::use_data(ESP_sf, overwrite = T)
} else {
  return(ESP_sf)
}
}
get_ESP_sf(save_clean = T)
