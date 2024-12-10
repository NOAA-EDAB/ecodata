## Zooplankton Index
raw.dir <- here::here("data-raw/")

# Define file paths for Calanus finmarchicus index and center of gravity
falcalfin <- "fallcalfinindex - Sarah Gaichas - NOAA Federal.rds"
sprcalfin <- "springcalfinindex - Sarah Gaichas - NOAA Federal.rds"
falcalfincog <- "fallcalfincog - Sarah Gaichas - NOAA Federal.rds"
sprcalfincog <- "springcalfincog - Sarah Gaichas - NOAA Federal.rds"

# Define file paths for Euphausiids index and center of gravity
faleuph <- "falleuphindex - Sarah Gaichas - NOAA Federal.rds"
spreuph <- "springeuphindex - Sarah Gaichas - NOAA Federal.rds"
faleuphcog <- "falleuphcog - Sarah Gaichas - NOAA Federal.rds"
spreuphcog <- "springeuphcog - Sarah Gaichas - NOAA Federal.rds"

# Define file paths for small copepods index and center of gravity
falsmallcope <- "fallsmallcopeSOEindex - Sarah Gaichas - NOAA Federal.rds"
sprsmallcope <- "springsmallcopeSOEindex - Sarah Gaichas - NOAA Federal.rds"
falsmallcopecog <- "fallsmallcopeSOEcog - Sarah Gaichas - NOAA Federal.rds"
sprsmallcopecog <- "springsmallcopeSOEcog - Sarah Gaichas - NOAA Federal.rds"

# Define file paths for large copepods index and center of gravity
fallgcope <- "falllgcopeALLindex - Sarah Gaichas - NOAA Federal.rds"
sprlgcope <- "springlgcopeALLindex - Sarah Gaichas - NOAA Federal.rds"
fallgcopecog <- "falllgcopeALLcog - Sarah Gaichas - NOAA Federal.rds"
sprlgcopecog <- "springlgcopeALLcog - Sarah Gaichas - NOAA Federal.rds"

# Define file paths for zooplankton volume index and center of gravity
falzoopvol <- "fallzoopvolindex - Sarah Gaichas - NOAA Federal.rds"
sprzoopvol <- "springzoopvolindex - Sarah Gaichas - NOAA Federal.rds"
falzoopvolcog <- "fallzoopvolcog - Sarah Gaichas - NOAA Federal.rds"
sprzoopvolcog <- "springzoopvolcog - Sarah Gaichas - NOAA Federal.rds"

get_zooplankton_index <- function(save_clean = F){

  # Load input files for Calanus finmarchicus index and center of gravity
  fallcalfin <- readRDS(file.path(raw.dir, falcalfin))
  springcalfin <- readRDS(file.path(raw.dir, sprcalfin))
  fallcalfincog <- readRDS(file.path(raw.dir, falcalfincog))
  springcalfincog <- readRDS(file.path(raw.dir, sprcalfincog))

  # Load input files for Euphausiids index and center of gravity
  falleuph <- readRDS(file.path(raw.dir, faleuph))
  springeuph <- readRDS(file.path(raw.dir, spreuph))
  falleuphcog <- readRDS(file.path(raw.dir, faleuphcog))
  springeuphcog <- readRDS(file.path(raw.dir, spreuphcog))

  # Load input files for Calanus finmarchicus index and center of gravity
  fallsmallcope <- readRDS(file.path(raw.dir, falsmallcope))
  springsmallcope <- readRDS(file.path(raw.dir, sprsmallcope))
  fallsmallcopecog <- readRDS(file.path(raw.dir, falsmallcopecog))
  springsmallcopecog <- readRDS(file.path(raw.dir, sprsmallcopecog))

  # Load input files for Calanus finmarchicus index and center of gravity
  falllgcope <- readRDS(file.path(raw.dir, fallgcope))
  springlgcope <- readRDS(file.path(raw.dir, sprlgcope))
  falllgcopecog <- readRDS(file.path(raw.dir, fallgcopecog))
  springlgcopecog <- readRDS(file.path(raw.dir, sprlgcopecog))

  # Load input files for Calanus finmarchicus index and center of gravity
  fallzoopvol <- readRDS(file.path(raw.dir, falzoopvol))
  springzoopvol <- readRDS(file.path(raw.dir, sprzoopvol))
  fallzoopvolcog <- readRDS(file.path(raw.dir, falzoopvolcog))
  springzoopvolcog <- readRDS(file.path(raw.dir, sprzoopvolcog))

  zooplankton_index<- rbind(fallcalfin, springcalfin, fallcalfincog, springcalfincog,
                            falleuph, springeuph, falleuphcog, springeuphcog, fallsmallcope,
                            springsmallcope, fallsmallcopecog, springsmallcopecog, falllgcope,
                            springlgcope, falllgcopecog, springlgcopecog, fallzoopvol,
                            springzoopvol, fallzoopvolcog, springzoopvolcog)

  if (save_clean){
    usethis::use_data(zooplankton_index, overwrite = T)
  } else {
    return(zooplankton_index)
  }
}
get_zooplankton_index(save_clean = T)
