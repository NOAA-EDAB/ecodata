## Chesapeake Bay SST

library(raster)
library(tidyverse)

raw.dir <- here::here("data-raw")

# Chesapeake bay sst files from Ron
ches_1_nc <-"ChesBaySST-seasonal-anomaly_Season1_2021vs2009-2020_RVogel - Ronald Vogel - NOAA Affiliate.nc4"
ches_2_nc <-"ChesBaySST-seasonal-anomaly_Season2_2021vs2009-2020_RVogel - Ronald Vogel - NOAA Affiliate.nc4"
ches_3_nc <-"ChesBaySST-seasonal-anomaly_Season3_2021vs2009-2020_RVogel - Ronald Vogel - NOAA Affiliate.nc4"
ches_4_nc <-"ChesBaySST-seasonal-anomaly_Season4_2021vs2009-2020_RVogel - Ronald Vogel - NOAA Affiliate.nc4"


get_ches_bay_sst <- function(save_clean = F){

  ches1<- raster::raster(file.path(raw.dir, ches_1_nc),varname = "sst_anomaly")
  ches2<- raster::raster(file.path(raw.dir, ches_2_nc),varname = "sst_anomaly")
  ches3<- raster::raster(file.path(raw.dir, ches_3_nc),varname = "sst_anomaly")
  ches4<- raster::raster(file.path(raw.dir, ches_4_nc),varname = "sst_anomaly")

  sst_rast<- raster::stack(ches1, ches2, ches3, ches4)
  raster::extent(sst_rast) <- raster::extent(c(-78,-74, 36, 40))
  crs(sst_rast) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83"

  r <- as(sst_rast, "SpatialPointsDataFrame") #Convert to ggplot-able object
  r <- as.data.frame(r)

  ches_bay_sst <- r %>%
    dplyr::rename(Winter = "sst.anomaly.2021.minus.2009.2020.1",
                  Spring = "sst.anomaly.2021.minus.2009.2020.2",
                  Summer = "sst.anomaly.2021.minus.2009.2020.3",
                  Fall = "sst.anomaly.2021.minus.2009.2020.4") %>%
    tidyr::pivot_longer(!c(x, y), names_to = "Season", values_to = "Value") %>%
    dplyr::rename(Longitude = "y",
                  Latitude = "x") %>%
    filter(!Value == "NaN")


  if (save_clean){
    usethis::use_data(ches_bay_sst, overwrite = T)
  } else {
    return(ches_bay_sst)
  }
}
get_ches_bay_sst(save_clean = T)


















sst<- ches_bay_sst

map.lwd <- 0.4

# Set lat/lon window for maps
xmin = -78
xmax = -74
ymin = 36
ymax = 40
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)
#sst <- ecodata::seasonal_sst_anomaly_gridded
crs<- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
sst$Season <- factor(sst$Season, levels = c("Winter",
                                            "Spring",
                                            "Summer",
                                            "Fall"))

sst<- sst %>% dplyr::mutate(Value = replace(Value, Value > 5, 5))
sst_map <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  ggplot2::scale_fill_gradient2(name = "Temp.\nAnomaly (C)",
                                low = scales::muted("blue"),
                                mid = "white",
                                high = scales::muted("red"),
                                limits = c(-5,5),
                                labels = c("<-5", "-2.5", "0", "2.5", ">5")) +
  ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +

  ggplot2::geom_tile(data = sst, aes(x = Latitude, y = Longitude,fill = Value)) +
  ggplot2::facet_wrap(Season~.) +
  ecodata::theme_map() +
  ggplot2::ggtitle("Chesapeake Bay SST anomaly (2021)") +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                 legend.key = element_blank(),
                 axis.title = element_text(size = 11),
                 strip.background = element_blank(),
                 strip.text=element_text(hjust=0),
                 axis.text = element_text(size = 8),
                 axis.title.y = element_text(angle = 90))+
  ecodata::theme_title()


sst_map
