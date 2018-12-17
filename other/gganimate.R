library(gganimate)
library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
library(gstat)
library(here)
library(raster)
library(rgdal)
library(spatstat)
library(maptools)
library(dismo)

clean.dir <- here("data") #output directory for cleaned data
gis.dir <- here("inst","extdata","gis")

#CRS
crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#Load
load(file.path(gis.dir,"LME_nutrients_spatial.Rdata"))
epu <- readOGR(file.path(gis.dir, "EPU_Extended.shp"), verbose = F) 
crs(epu) <- crs

#Filter variable
nuts <- LME_nutrients_spatial %>% filter(Var == "NITRIT.NITRAT") %>%
  mutate(ID = group_indices_(., .dots=c("Latitude", "Longitude")))

#Pull out lat lon for merging later
latlon <- nuts %>% dplyr::select(Latitude, Longitude, ID) %>% distinct() 

#Summarize
nuts %<>% 
  filter(Value != -999) %>% 
  group_by(Year = year(Time),
           Month = month(Time),
           ID) %>% 
  dplyr::summarise(Value = mean(as.numeric(Value), na.rm = T)) %>% 
  left_join(.,latlon,by = "ID")

#create empty raster
r1 <- raster::raster()
e <- raster::extent(-77, -65.66667, 35.8327, 44.66667)
raster::extent(r1) <- e

#fill with strata
r1 <- raster::rasterize(epu, r1, field = 1, fun = mean)

ggout <- NULL
for (i in 1:length(unique(nuts$Year))){
  #Convert to SPDF for one year to experiment
  print(unique(nuts$Year)[i])
   rsub_df <- nuts %>% filter(Year == unique(nuts$Year)[i]) %>%
  # rsub_df <- nuts %>% filter(Year == 2017) %>%
    ungroup() %>% 
    dplyr::select(Latitude, Longitude, Value) %>% 
    as.data.frame()
  
  rsub <- rsub_df
  
  coordinates(rsub) <- ~Longitude + Latitude
  rsub@bbox <- epu@bbox
  crs(rsub) <- crs

  #Nearest neighbor interpolation ------------------------------------------------
  
  
  gs <- gstat(formula = Value~1, locations = rsub, nmax = 2, set = list(idp = 0))
  nn <- interpolate(r1, gs)
  nnmsk <- mask(nn, r1)
  
  #Convert IDW output for ggplot
  r_spdf <- as(nnmsk, "SpatialPixelsDataFrame")
  r_df <- as.data.frame(r_spdf)
  m_df <- r_df %>%
    reshape2::melt(id = c("y","x")) %>%
    dplyr::rename(lat = y, long = x) %>%
    dplyr::select(-variable)
  
  m_df$year <- unique(nuts$Year)[i]

  assign('ggout',rbind(ggout,m_df))
}



#Tesselate-------------------------------------------------------------------
# tess <- as(dirichlet(as.ppp(rsub)), "SpatialPolygons")
# 
# #Reload projection
# proj4string(tess) <- proj4string(rsub)
# 
# #Use over() to join point attributes to tesselated surface 
# tess_df <- over(tess, rsub, fn = mean)
# tess_spdf <- SpatialPolygonsDataFrame(tess, tess_df)
# 
# #Clip tessellated surface to EPU boundaries
# tess_out <- raster::intersect(epu, tess_spdf)
# 
# tess_out@data$id <- rownames(tess_out@data)
# tess_points <- fortify(tess_out, region = "id")
# tess <-plyr::join(tess_points, tess_out@data, by = "id")
# 
# ggplot() +
#     geom_polygon(data = tess, aes(x = long, y = lat, group = group, fill = Value))+
#     geom_point(data = nuts, aes(x = Longitude, y = Latitude))



# plot(nnmsk)

#Inverse distance weighting
# gs_idw <- gstat(formula = Value~1, locations = rsub)
# idw <- interpolate(epu, gs_idw)
# idwr <- mask(idw, bounded_rast)
# plot(idwr)


#ggplot----------------------------------------------------------------

xmin = -76
xmax = -66
ymin = 36
ymax = 45
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)

ne_countries <- rnaturalearth::ne_countries(scale = 10,
                                            continent = "North America",
                                            returnclass = "sf") %>%
  sf::st_transform(crs = crs)

# %>%

#Plotting
ggplot(data = ggout, aes(x = long, y = lat, fill = value))+
  geom_tile() +
  geom_sf(data = ne_countries,aes(),inherit.aes = FALSE,
          fill = "white",
          color = "black",
          size = 0.25) +
  ease_aes('linear')+
  labs(title = 'Year: {closest_state}', x= 'Longitude', y  = 'Latitude',
       fill = paste("Nitrate +\n Nitrite (\u03BCM)"))+
  transition_states(as.integer(year),
                    
                    transition_length = 2,
                    state_length = 10,) +
  scale_fill_gradient2(low = "blue",mid = "white",high = "red",
                       midpoint = median(ggout$value))+
  coord_sf(crs = crs, xlim = xlims, ylim = ylims)#+
 # geom_point(data = rsub_df, aes(x = Longitude, y = Latitude))
  
ggout %>% filter(year == 2013) %>% 
  ggplot()+
  geom_tile(aes(x = long, y = lat, fill = value))+
  labs(fill = paste("Nitrate +\n Nitrite (\u03BCM)"))

last_animation()




ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 1,
    state_length = 10
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
