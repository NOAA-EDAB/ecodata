# Process optimally interpolated EcoMon zooplankton data
#
# These data show estimated zooplankton abundances on the NE Shelf
# derived from Ecosystem Monitoring Program (EcoMon)
# sampling. EcoMon conducts shelf-wide bimonthly surveys of the Northeast
# Large Marine Ecosystem, collecting
# and ichthyoplankton to a depth of 200 m using paired Bongo samplers with
# 333 $\mu$m mesh netting. Zooplankton
# abundance data were interpolated across sampling locations using ordinary
# kriging to create a complete field.
# Here we present abundance time series for three species:
# *Centropages typicus*, *Temora longicornis*, and
# *Pseudocalanus* spp. These data are processed in a similar
# manner to optimally interpolated ocean temperature and
# salinity data sets. More information about the source data
# and processing methods used to derive these data sets
# can be found at https://noaa-edab.github.io/ECSA/#sec:methodszoo.

library(tidyverse)
raw.dir<-here::here("data-raw")
source(file.path(raw.dir, "process_oi.R"))

if (!all(exists("mab_rast"),
         exists("gb_rast"),
         exists("gom_rast"))){
  message("Loading EPU rasters.")
  source(file.path(raw.dir, "create_epu_mask_oi.R"))
} else {
  message("All EPU rasters exist; skipping load step.")
}

get_zoo_oi <- function(save_clean = F){

  #MAB---------------------------------------------------------------------------------------------------
  ctyp_fall_mab <- process_oi(variable = "zooplankton",
                              genus = "centropages",
                              season = "fall", epu = "MAB")
  ctyp_spring_mab <- process_oi(variable = "zooplankton",
                                genus = "centropages",
                                season = "spring", epu = "MAB")

  tlong_fall_mab <- process_oi(variable = "zooplankton",
                               genus = "temora",
                               season = "fall", epu = "MAB")
  tlong_spring_mab <- process_oi(variable = "zooplankton",
                                 genus = "temora",
                                 season = "spring", epu = "MAB")

  pseudo_fall_mab <- process_oi(variable = "zooplankton",
                                genus = "pseudocalanus",
                                season = "fall", epu = "MAB")
  pseudo_spring_mab <- process_oi(variable = "zooplankton",
                                  genus = "pseudocalanus",
                                  season = "spring", epu = "MAB")


  #GB---------------------------------------------------------------------------------------------------
  ctyp_fall_gb <- process_oi(variable = "zooplankton",
                             genus = "centropages",
                             season = "fall", epu = "GB")
  ctyp_spring_gb <- process_oi(variable = "zooplankton",
                               genus = "centropages",
                               season = "spring", epu = "GB")

  tlong_fall_gb <- process_oi(variable = "zooplankton",
                              genus = "temora",
                              season = "fall", epu = "GB")
  tlong_spring_gb <- process_oi(variable = "zooplankton",
                                genus = "temora",
                                season = "spring", epu = "GB")

  pseudo_fall_gb <- process_oi(variable = "zooplankton",
                               genus = "pseudocalanus",
                               season = "fall", epu = "GB")
  pseudo_spring_gb <- process_oi(variable = "zooplankton",
                                 genus = "pseudocalanus",
                                 season = "spring", epu = "GB")


  #GOM---------------------------------------------------------------------------------------------------
  ctyp_fall_gom <- process_oi(variable = "zooplankton",
                              genus = "centropages",
                              season = "fall", epu = "GOM")
  ctyp_spring_gom <- process_oi(variable = "zooplankton",
                                genus = "centropages",
                                season = "spring", epu = "GOM")

  tlong_fall_gom <- process_oi(variable = "zooplankton",
                               genus = "temora",
                               season = "fall", epu = "GOM")
  tlong_spring_gom <- process_oi(variable = "zooplankton",
                                 genus = "temora",
                                 season = "spring", epu = "GOM")

  pseudo_fall_gom <- process_oi(variable = "zooplankton",
                                genus = "pseudocalanus",
                                season = "fall", epu = "GOM")
  pseudo_spring_gom <- process_oi(variable = "zooplankton",
                                  genus = "pseudocalanus",
                                  season = "spring", epu = "GOM")

  #Bind all zoo output. Be sure to include any new species here
  zoo_oi <- NULL
  print(ls())
  for (i in ls()){
    if (stringr::str_detect(i,"ctyp|tlong|pseudo")){
      assign("zoo_oi", rbind(zoo_oi, get(i)))
    }
  }

  #Process output from masked rasters (mean, +/- 1 SD). Adjust output in process_oi(), not here.
  zoo_oi <- zoo_oi %>%
    dplyr::rename(EPU = epu) %>%
    dplyr::select(-Season) %>%
    mutate(Units = "log N m^-3")

  #Split out +/- SD because too lazy to gather ;)
  sd.low <- zoo_oi %>%
    dplyr::select(-Value, -sd.high) %>%
    mutate(Var = as.factor(paste(Var, "- 1 SD"))) %>%
    dplyr::rename(Value = sd.low)

  sd.high <- zoo_oi %>%
    dplyr::select(-Value, -sd.low) %>%
    mutate(Var = as.factor(paste(Var, "+ 1 SD"))) %>%
    dplyr::rename(Value = sd.high)

  zoo_oi <- zoo_oi %>% dplyr::select(-sd.high,-sd.low)

  #Bind all data
  zoo_oi <- rbind(zoo_oi, sd.high, sd.low) %>% as.data.frame()

  if (save_clean){
    usethis::use_data(zoo_oi, overwrite = T)
  } else {
    return(zoo_oi)
  }

}
get_zoo_oi(save_clean = T)
