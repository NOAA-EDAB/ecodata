# Process optimally interpolated ocean salinity data for the Northeast Continental Shelf
# 
# These data show optimally interpolated sea-surface and bottom salinities between 1992-2017 
# (methods for OI procedure are available at https://noaa-edab.github.io/ECSA/. Processing of 
# these time series follows the same methods used to process optimally interpolated ocean
# temperatures. These data are presented as RasterStacks (i.e. one raster layer per year),
# which are then masked by EPU polygons for further analyses. Here we take the annual mean of
# masked raster layers to create a time series of OI observations.

r.dir <- here::here("data-raw")
source(file.path(r.dir, "process_oi.R"))

if (!all(exists("mab_rast"),
         exists("gb_rast"),
         exists("gom_rast"))){
  message("Loading EPU rasters.")
  source(file.path(r.dir, "create_epu_mask_oi.R"))
} else {
  message("All EPU rasters exist; skipping load step.")
}


get_oceansal_oi <- function(save_clean = F){
  #Bottom salinities
  bsal_fall_mab <- process_oi(variable = "salinity", type = "bottom", season = "fall", epu = "MAB")
  bsal_spring_mab <- process_oi(variable = "salinity", type = "bottom", season = "spring", epu = "MAB")
  
  bsal_fall_gb <- process_oi(variable = "salinity", type = "bottom", season = "fall", epu = "GB")
  bsal_spring_gb <- process_oi(variable = "salinity", type = "bottom", season = "spring", epu = "GB")
  
  bsal_fall_gom <- process_oi(variable = "salinity", type = "bottom", season = "fall", epu = "GOM")
  bsal_spring_gom <- process_oi(variable = "salinity", type = "bottom", season = "spring", epu = "GOM")
  
  
  #Surface salinities
  ssal_fall_mab <- process_oi(variable = "salinity", type = "surface", season = "fall", epu = "MAB")
  ssal_spring_mab <- process_oi(variable = "salinity", type = "surface", season = "spring", epu = "MAB")
  
  ssal_fall_gb <- process_oi(variable = "salinity", type = "surface", season = "fall", epu = "GB")
  ssal_spring_gb <- process_oi(variable = "salinity", type = "surface", season = "spring", epu = "GB")
  
  ssal_fall_gom <- process_oi(variable = "salinity", type = "surface", season = "fall", epu = "GOM")
  ssal_spring_gom <- process_oi(variable = "salinity", type = "surface", season = "spring", epu = "GOM")
  
  
  bottom_sal_oi <- rbind(bsal_fall_mab,
                         bsal_spring_mab,
                         bsal_fall_gb,
                         bsal_spring_gb,
                         bsal_fall_gom,
                         bsal_spring_gom)
  
  surface_sal_oi <- rbind(ssal_fall_mab,
                          ssal_spring_mab,
                          ssal_fall_gb,
                          ssal_spring_gb,
                          ssal_fall_gom,
                          ssal_spring_gom)
  
  #Start with annual mean, generating sampling uncertainty for this time series is possible
  bottom_sal_oi_annual <- bottom_sal_oi %>% 
    group_by(epu, Time) %>% 
    dplyr::summarise(Value = mean(Value)) %>% 
    dplyr::rename(EPU = epu) %>% 
    dplyr::mutate(Units = "PSU", Var = "bottom sal OI") 
  
  
  #Same thing for annual salinity
  surface_sal_oi_annual <- surface_sal_oi %>% 
    group_by(epu, Time) %>% 
    dplyr::summarise(Value = mean(Value)) %>% 
    dplyr::rename(EPU = epu) %>% 
    dplyr::mutate(Units = "PSU", Var = "surface sal OI")
  
  ocean_sal_oi <- rbind(bottom_sal_oi_annual, surface_sal_oi_annual)
  
  if (save_clean){
    save(ocean_sal_oi,file =
           file.path(clean.dir, "ocean_sal_oi.Rds"))
  } else {
    return(ocean_sal_oi)
  }
  
}
