
dat<- ecodata::long_line_survey %>%
  dplyr::filter(COMMON_NAME == "BLACKBELLY ROSEFISH")

nesbath <- marmap::fortify.bathy(marmap::getNOAA.bathy(lon1 = -72, lon2 = -65, lat1 = 40, lat2 = 45,resolution = 5))

#invisible(sf::st_crs(poly)<-crs)
colors <- c("A" = "orange", "B" = "blue")
ggplot2::ggplot() +
  ggplot2::geom_raster(data = nesbath, aes(x=x,y=y, fill = z)) +
  ggplot2::scale_fill_gradientn(colors =c("lightcyan","lightblue4"))+
  ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  ggplot2::geom_sf(data = epu_sf, fill = "transparent", size = map.lwd) +
  ggplot2::geom_sf(data = ecodata::longlinesurvey_sf, size = map.lwd, color = "black",fill = "transparent")+
  ggplot2::geom_point(data = dat, aes(x = DECDEG_BEGLON_SET, y = DECDEG_BEGLAT_SET,color = categ), size = map.lwd)+
  ggplot2::coord_sf(crs = crs, xlim = c(-72, -66.5), ylim = c(41, 44.5))+
  ggspatial::annotation_scale(location = "br", width_hint = 0.4) +
  ggplot2::scale_color_manual(values = colors, breaks = c("A", "B"),
                              labels = c("2014-2017", "2018-2021"))+
  ggplot2::theme_bw( ) +
  ggplot2::ylab("")+
  ggplot2::xlab("")+
  ggplot2::theme(legend.title = element_blank())+
  ggplot2::ggtitle("BlackBelly Rose collected in GOM Long-Line Survey")
