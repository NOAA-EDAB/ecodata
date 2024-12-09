### Thermal Habitat Persistence - Joe Caracappa

# install.packages('rerddap')
library(dplyr)
library(usethis)
library(rerddap)
library(terra)

get_thermal_habitat_gridded <- function(threshold, year, depth.bins, save_clean = F){

  yt.combs = expand.grid(year = year, threshold = threshold)%>%
    dplyr::mutate(threshold.char = gsub('\\.','_',threshold))

  #Read EPU shp
  epu.mask = terra::vect(here::here('data-raw','gis','EPU_NOESTUARIES.shp'))

  #Read Bathy Shp
  depth.mask = terra::rast(here::here('data-raw','gis','GLORYS_depth_NEUS.nc'))

  #Get combinations of EPU and depth bin
  depth.df = data.frame(depth.min = depth.bins[-length(depth.bins)],
                        depth.max = depth.bins[-1])%>%
    dplyr::mutate(depth.id = 1:n(),
                  depth.name = paste0(depth.min,'-',depth.max,'m'))

  epu.depth.combs = expand.grid(epu.name = epu.mask$EPU,
                                depth.id = depth.df$depth.id)%>%
    dplyr::left_join(depth.df)

  df.ind = 1
  output.ls = list()

  #Loop through year x threshold combinations
  for(i in 1:nrow(yt.combs)){

    #Download from ERDDAP
    file.url = URLencode(paste0('http://nefsctest.nmfs.local:8080/erddap/griddap/annual_thermal_habitat_glorys.nc?nday_',yt.combs$threshold.char[i],'%5B(',yt.combs$year[i],'-01-01):1:(',yt.combs$year[i],'-01-01)%5D%5B(47.0):1:(34.0)%5D%5B(-78.0):1:(-62.00000000000001)%5D'),repeated = F,reserved = T)
    data.file.name = here::here('data-raw',paste0('thermal_habitat_gridded_',yt.combs$threshold.char[i],'_',yt.combs$year[i],'.nc'))
    system.download = paste('curl --compressed -g "',file.url,'" -o ',data.file.name,sep = '')
    system(system.download)

    #Convert to raster object
    comb.data = terra::rast(data.file.name)

    #Loop through EPU x depth bins
    for(j in 1:nrow(epu.depth.combs)){

      # Create mask EPU x depth
      this.epu = epu.mask[which(epu.mask$EPU == epu.depth.combs$epu.name[j])]
      depth.range = terra::clamp(depth.mask,lower = epu.depth.combs$depth.min[j], upper = epu.depth.combs$depth.max[j], values = F)
      depth.epu.rast = terra::crop(terra::mask(depth.range,this.epu),this.epu)

      # convert to dataframe
      output.ls[[df.ind]] = as.data.frame(depth.epu.rast,xy =T) %>%
        dplyr::rename(Value = 'GLORYS_depth_NEUS',
                      Longitude = 'x',
                      Latitude = 'y')%>%
        dplyr::mutate(Time = yt.combs$year[i],
                      EPU = epu.depth.combs$epu.name[j],
                      Depth = epu.depth.combs$depth.name[j],
                      Var = yt.combs$threshold[i],
                      Source = 'GLORYS',
                      Units = 'Number of Days above threshold')%>%
        dplyr::select(Time, EPU, Depth, Var, Value, Latitude, Longitude, Source, Units)

      df.ind = df.ind +1
    }

    #Delete temp file
    file.remove(data.file.name)

  }

  thermal_habitat_gridded = dplyr::bind_rows(output.ls)

  if (save_clean){
    usethis::use_data(thermal_habitat_gridded, overwrite = T)
  } else {
    return(thermal_habitat_gridded)
  }
}
get_thermal_habitat_gridded(threshold = c(15,18.5,24),
                            year = 2023,
                            depth.bins = c(0,25,100,3000),
                            save_clean = T)



