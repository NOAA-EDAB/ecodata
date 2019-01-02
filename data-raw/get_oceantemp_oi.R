# Process optimally interpolated ocean temperature data for the Northeast Continental Shelf

# These data show NE shelf surface and bottom temperatures estimated through an optimal interpolation
# procedure (see methods at https://noaa-edab.github.io/ECSA). Spring and fall time series were 
# standardized to April 3 and October 11 between the period of 1968-2017. The code below first creates 
# a downsampled raster of each Ecological Production Unit (build_epu.R). These downsampled EPU rasters are then used
# to mask SST and bottom temperature data, from which annual means are computed. This same approach is
# used to generate time series from EcoMon zooplankton sampling and optimally interpolated salinity data.

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


get_oceantemp_oi <- function(save_clean = F){
  
  

  #Bottom temperatures
  bt_fall_mab <- process_oi(variable = "temperature", type = "bottom", season = "fall", epu = "MAB")
  bt_spring_mab <- process_oi(variable = "temperature", type = "bottom", season = "spring", epu = "MAB")
  
  bt_fall_gb <- process_oi(variable = "temperature", type = "bottom", season = "fall", epu = "GB")
  bt_spring_gb <- process_oi(variable = "temperature", type = "bottom", season = "spring", epu = "GB")
  
  bt_fall_gom <- process_oi(variable = "temperature", type = "bottom", season = "fall", epu = "GOM")
  bt_spring_gom <- process_oi(variable = "temperature", type = "bottom", season = "spring", epu = "GOM")
  
  
  #Surface temperatures
  sst_fall_mab <- process_oi(variable = "temperature", type = "surface", season = "fall", epu = "MAB")
  sst_spring_mab <- process_oi(variable = "temperature", type = "surface", season = "spring", epu = "MAB")
  
  sst_fall_gb <- process_oi(variable = "temperature", type = "surface", season = "fall", epu = "GB")
  sst_spring_gb <- process_oi(variable = "temperature", type = "surface", season = "spring", epu = "GB")
  
  sst_fall_gom <- process_oi(variable = "temperature", type = "surface", season = "fall", epu = "GOM")
  sst_spring_gom <- process_oi(variable = "temperature", type = "surface", season = "spring", epu = "GOM")
  
  
  bottom_temp_oi <- rbind(bt_fall_mab,
                          bt_spring_mab,
                          bt_fall_gb,
                          bt_spring_gb,
                          bt_fall_gom,
                          bt_spring_gom)
  
  surface_temp_oi <- rbind(sst_fall_mab,
                           sst_spring_mab,
                           sst_fall_gb,
                           sst_spring_gb,
                           sst_fall_gom,
                           sst_spring_gom)
  
  #Start with annual mean, generating sampling uncertainty for this time series is possible
  bottom_temp_oi_annual <- bottom_temp_oi %>% 
    group_by(epu, Time) %>% 
    dplyr::summarise(Value = mean(Value)) %>% 
    dplyr::rename(EPU = epu) %>% 
    dplyr::mutate(Units = "degreesC", Var = "bottom temp OI") 
  
  
  #Same thing for annual SST
  surface_temp_oi_annual <- surface_temp_oi %>% 
    group_by(epu, Time) %>% 
    dplyr::summarise(Value = mean(Value)) %>% 
    dplyr::rename(EPU = epu) %>% 
    dplyr::mutate(Units = "degreesC", Var = "surface temp OI")
  
  ocean_temp_oi <- rbind(bottom_temp_oi_annual, surface_temp_oi_annual)
  
  if (save_clean){
    save(ocean_temp_oi,file =
           file.path(clean.dir, "ocean_temp_oi.Rds"))
  } else {
    return(ocean_temp_oi)
  }
  
}

