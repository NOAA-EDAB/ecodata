
nesbath <- marmap::fortify.bathy(marmap::getNOAA.bathy(lon1 = -77, lon2 = -65, lat1 = 35, lat2 = 45,
              resolution = 5))

lease_s<-sf::st_read(file.path(gis.dir, 'BOEM lease areas/ne_existing_leasesSPoly.shp'))
lease_n<-sf::st_read(file.path(gis.dir, 'BOEM lease areas/ne_existing_leasesNPoly.shp'))
lease_m<-sf::st_read(file.path(gis.dir, 'BOEM lease areas/ne_existing_leasesMPoly.shp'))
prop_n<-sf::st_read(file.path(gis.dir, 'BOEM lease areas/ne_proposed_leases_NPoly.shp'))
prop_m<-sf::st_read(file.path(gis.dir, 'BOEM lease areas/ne_proposed_leases_MPoly.shp'))

invisible(sf::st_crs(lease_s)<-crs)
invisible(sf::st_crs(lease_n)<-crs)
invisible(sf::st_crs(lease_m)<-crs)
invisible(sf::st_crs(prop_n)<-crs)
invisible(sf::st_crs(prop_m)<-crs)

ggplot2::ggplot() +
  ggplot2::geom_raster(data = nesbath, aes(x=x,y=y, fill = z)) +
  ggplot2::scale_fill_gradientn(colors =c("lightcyan","lightblue4"))+
  ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  ggplot2::geom_sf(data = epu_sf, fill = "transparent", size = map.lwd) +
  ggplot2::geom_sf(data = lease_s, size = map.lwd, color = "black")+
  ggplot2::geom_sf(data = lease_n, size = map.lwd, color = "black")+
  ggplot2::geom_sf(data = lease_m, size = map.lwd, color = "black")+
  ggplot2::geom_sf(data = prop_n, size = map.lwd, color = "red3")+
  ggplot2::geom_sf(data = prop_m, size = map.lwd, color = "red3")+
  ggplot2::coord_sf(crs = crs, xlim = c(-77, -69), ylim = c(36,42))+
  ggplot2::geom_segment(aes(x = -74.6, y = 37.4, xend =-75.4, yend =38), colour = "blue4")+
  ggplot2::geom_segment(aes(x = -71.1, y = 40.2, xend =-71.6, yend =41.1), colour = "blue4")+
  ggplot2::annotate("text", x = -74.9, y = 37, label = "S")+
  ggplot2::annotate("text", x = -73.5, y = 38.7, label = "M")+
  ggplot2::annotate("text", x = -70.5, y = 40.2, label = "N")+
  ggspatial::annotation_scale(location = "br", width_hint = 0.4) +
  ggplot2::theme_bw( ) +
  ggplot2::ylab("")+
  ggplot2::xlab("")+
  ggplot2::theme(legend.position = "none") +
  ggplot2::ggtitle("BOEM lease areas")
