
library(rnaturalearthhires)
library(sf)
library(raster)
library(ggspatial)
library(marmap)

crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
gis.dir <- here::here("data-raw/gis")
nesbath <- fortify.bathy(getNOAA.bathy(lon1 = -77, lon2 = -65, lat1 = 35, lat2 = 45,
              resolution = 5))

lease_s<-st_read(file.path(gis.dir, 'BOEM lease areas/ne_existing_leasesSPoly.shp'))
lease_n<-st_read(file.path(gis.dir, 'BOEM lease areas/ne_existing_leasesNPoly.shp'))
lease_m<-st_read(file.path(gis.dir, 'BOEM lease areas/ne_existing_leasesMPoly.shp'))
prop_n<-st_read(file.path(gis.dir, 'BOEM lease areas/ne_proposed_leases_NPoly.shp'))
prop_m<-st_read(file.path(gis.dir, 'BOEM lease areas/ne_proposed_leases_MPoly.shp'))

invisible(st_crs(lease_s)<-crs)
invisible(st_crs(lease_n)<-crs)
invisible(st_crs(lease_m)<-crs)
invisible(st_crs(prop_n)<-crs)
invisible(st_crs(prop_m)<-crs)

ggplot() +
  geom_raster(data = nesbath, aes(x=x,y=y, fill = z)) +
  scale_fill_gradientn(colors =c("lightcyan","lightblue4"))+
  geom_sf(data = coast, size = map.lwd) +
  geom_sf(data = epu_sf, fill = "transparent", size = map.lwd) +
  geom_sf(data = lease_s, size = map.lwd, color = "black")+
  geom_sf(data = lease_n, size = map.lwd, color = "black")+
  geom_sf(data = lease_m, size = map.lwd, color = "black")+
  geom_sf(data = prop_n, size = map.lwd, color = "red3")+
  geom_sf(data = prop_m, size = map.lwd, color = "red3")+
  coord_sf(crs = crs, xlim = c(-77, -69), ylim = c(36,42))+
  geom_segment(aes(x = -74.6, y = 37.4, xend =-75.4, yend =38), colour = "blue4")+
  geom_segment(aes(x = -71.1, y = 40.2, xend =-71.6, yend =41.1), colour = "blue4")+
  annotation_scale(location = "br", width_hint = 0.4) +
  theme_bw( ) +
  ylab("")+
  xlab("")+
  theme(legend.position = "none") +
  ggtitle("BOEM lease areas")
