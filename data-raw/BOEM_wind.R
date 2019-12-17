### Plot BOEM lease and planned areas on epu_extended

library(sf)
library(rgdal)
library(raster)
library(rnaturalearth)

gis.dir <- here::here('data-raw','gis')
lease_shp <- readOGR(file.path(gis.dir, "BOEM_Lease_Areas_2_13_2019.shp"), verbose = F)
planned_shp <- readOGR(file.path(gis.dir, "BOEM_MHK_Planning_Areas_2_13_2019.shp"), verbose = F)

crs(lease_shp) <- crs
lease_sf <- as(lease_shp, "sf")

crs(planned_shp) <- crs
planned_sf <- as(planned_shp, "sf")

ggplot() +
  geom_sf(data = coast, size = map.lwd) +
  geom_sf(data  = ne_states, size = map.lwd) +
  geom_sf(data = lease_sf, size = map.lwd) +
  coord_sf(crs = crs, xlim = xlims, ylim = ylims)

ggplot() +
  geom_sf(data = coast, size = map.lwd) +
  geom_sf(data  = ne_states, size = map.lwd) +
  geom_sf(data = planned_sf, size = map.lwd) +
  coord_sf(crs = crs, xlim = xlims, ylim = ylims)
