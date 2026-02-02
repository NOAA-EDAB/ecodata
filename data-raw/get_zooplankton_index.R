## Zooplankton Index
raw.dir <- here::here("data-raw/")

# Define file paths for Calanus finmarchicus index and center of gravity
falcalfin <- "fall_calfin_index_2025-11-25.rds"
sprcalfin <- "spring_calfin_index_2025-11-25.rds"
falcalfincog <- "fall_calfin_cog_2025-11-25.rds"
sprcalfincog <- "spring_calfin_cog_2025-11-25.rds"

# Define file paths for Euphausiids index and center of gravity
faleuph <- "fall_euph_index_2025-11-25.rds"
spreuph <- "spring_euph_index_2025-11-25.rds"
faleuphcog <- "fall_euph_cog_2025-11-25.rds"
spreuphcog <- "spring_euph_cog_2025-11-25.rds"

# Define file paths for small copepods index and center of gravity
falsmallcope <- "fall_smcope_index_2025-11-25.rds"
sprsmallcope <- "spring_smcope_index_2025-11-25.rds"
falsmallcopecog <- "fall_smcope_cog_2025-11-25.rds"
sprsmallcopecog <- "spring_smcope_cog_2025-11-25.rds"

# Define file paths for large copepods index and center of gravity
fallgcope <- "fall_lgcope_index_2025-11-25.rds"
sprlgcope <- "spring_lgcope_index_2025-11-25.rds"
fallgcopecog <- "fall_lgcope_cog_2025-11-25.rds"
sprlgcopecog <- "spring_lgcope_cog_2025-11-25.rds"

# Define file paths for zooplankton volume index and center of gravity
falzoopvol <- "fallzoopvolindex - Sarah Gaichas - NOAA Federal.rds"
sprzoopvol <- "springzoopvolindex - Sarah Gaichas - NOAA Federal.rds"
falzoopvolcog <- "fallzoopvolcog - Sarah Gaichas - NOAA Federal.rds"
sprzoopvolcog <- "springzoopvolcog - Sarah Gaichas - NOAA Federal.rds"

# Define file paths for small copepods index and center of gravity
falsmallcopeALL <- "fallsmallcopeALLindex - Sarah Gaichas - NOAA Federal.rds"
sprsmallcopeALL <- "spring_smcopeall_index_2025-12-16.rds"
falsmallcopecogALL <- "fallsmallcopeALLcog - Sarah Gaichas - NOAA Federal.rds"
sprsmallcopecogALL <- "spring_smcopeall_cog_2025-12-16.rds"

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

  # Load input files for small copepods index and center of gravity
  fallsmallcope <- readRDS(file.path(raw.dir, falsmallcope)) |>
    dplyr::mutate(Var = dplyr::recode(Var,
                                      "Fall Smcope Abundance Index Estimate" = "Fall Smallcopesoe Abundance Index Estimate",
                                      "Fall Smcope Abundance Index Estimate SE" = "Fall Smallcopesoe Abundance Index Estimate SE"))
  springsmallcope <- readRDS(file.path(raw.dir, sprsmallcope)) |>
    dplyr::mutate(Var = dplyr::recode(Var,
                                      "Spring Smcope Abundance Index Estimate" = "Spring Smallcopesoe Abundance Index Estimate",
                                      "Spring Smcope Abundance Index Estimate SE" = "Spring Smallcopesoe Abundance Index Estimate SE"))
  fallsmallcopecog <- readRDS(file.path(raw.dir, falsmallcopecog)) |>
    dplyr::mutate(Var = dplyr::recode(Var,
                                      "Fall Smcope Eastward Center of Gravity" = "Fall Smallcopesoe Eastward Center of Gravity",
                                      "Fall Smcope Eastward Center of Gravity SE" = "Fall Smallcopesoe Eastward Center of Gravity SE",
                                      "Fall Smcope Northward Center of Gravity" = "Fall Smallcopesoe Northward Center of Gravity",
                                      "Fall Smcope Northward Center of Gravity SE" = "Fall Smallcopesoe Northward Center of Gravity SE"))
  springsmallcopecog <- readRDS(file.path(raw.dir, sprsmallcopecog)) |>
    dplyr::mutate(Var = dplyr::recode(Var,
                                      "Spring Smcope Eastward Center of Gravity" = "Spring Smallcopesoe Eastward Center of Gravity",
                                      "Spring Smcope Eastward Center of Gravity SE" = "Spring Smallcopesoe Eastward Center of Gravity SE",
                                      "Spring Smcope Northward Center of Gravity" = "Spring Smallcopesoe Northward Center of Gravity",
                                      "Spring Smcope Northward Center of Gravity SE" = "Spring Smallcopesoe Northward Center of Gravity SE"))

  # Load input files for large copepods index and center of gravity
  falllgcope <- readRDS(file.path(raw.dir, fallgcope)) |>
    dplyr::mutate(Var = dplyr::recode(Var,
                                      "Fall Lgcope Abundance Index Estimate" = "Fall Lgcopeall Abundance Index Estimate",
                                      "Fall Lgcope Abundance Index Estimate SE" = "Fall Lgcopeall Abundance Index Estimate SE"))
  springlgcope <- readRDS(file.path(raw.dir, sprlgcope)) |>
    dplyr::mutate(Var = dplyr::recode(Var,
                                      "Spring Lgcope Abundance Index Estimate" = "Spring Lgcopeall Abundance Index Estimate",
                                      "Spring Lgcope Abundance Index Estimate SE" = "Spring Lgcopeall Abundance Index Estimate SE"))
  falllgcopecog <- readRDS(file.path(raw.dir, fallgcopecog)) |>
    dplyr::mutate(Var = dplyr::recode(Var,
                                      "Fall Lgcope Eastward Center of Gravity" = "Fall Lgcopeall Eastward Center of Gravity",
                                      "Fall Lgcope Eastward Center of Gravity SE" = "Fall Lgcopeall Eastward Center of Gravity SE",
                                      "Fall Lgcope Northward Center of Gravity" = "Fall Lgcopeall Northward Center of Gravity",
                                      "Fall Lgcope Northward Center of Gravity SE" = "Fall Lgcopeall Northward Center of Gravity SE"))
  springlgcopecog <- readRDS(file.path(raw.dir, sprlgcopecog)) |>
    dplyr::mutate(Var = dplyr::recode(Var,
                                      "Spring Lgcope Eastward Center of Gravity" = "Spring Lgcopeall Eastward Center of Gravity",
                                      "Spring Lgcope Eastward Center of Gravity SE" = "Spring Lgcopeall Eastward Center of Gravity SE",
                                      "Spring Lgcope Northward Center of Gravity" = "Spring Lgcopeall Northward Center of Gravity",
                                      "Spring Lgcope Northward Center of Gravity SE" = "Spring Lgcopeall Northward Center of Gravity SE"))

  # Load input files for zooplankton volume index and center of gravity
  fallzoopvol <- readRDS(file.path(raw.dir, falzoopvol))
  springzoopvol <- readRDS(file.path(raw.dir, sprzoopvol))
  fallzoopvolcog <- readRDS(file.path(raw.dir, falzoopvolcog))
  springzoopvolcog <- readRDS(file.path(raw.dir, sprzoopvolcog))

  # Load input files for small copepods index and center of gravity
  fallsmallcopeALL <- readRDS(file.path(raw.dir, falsmallcopeALL))
  springsmallcopeALL <- readRDS(file.path(raw.dir, sprsmallcopeALL)) |>
    dplyr::mutate(Var = dplyr::recode(Var,
                                      "Spring Smcopeall Abundance Index Estimate" = "Spring Smallcopeall Abundance Index Estimate",
                                      "Spring Smcopeall Abundance Index Estimate SE" = "Spring Smallcopeall Abundance Index Estimate SE"))
  fallsmallcopeALLcog <- readRDS(file.path(raw.dir, falsmallcopecogALL))
  springsmallcopeALLcog <- readRDS(file.path(raw.dir, sprsmallcopecogALL)) |>
    dplyr::mutate(Var = dplyr::recode(Var,
                                      "Spring Smcopeall Eastward Center of Gravity" = "Spring Smallcopeall Eastward Center of Gravity",
                                      "Spring Smcopeall Eastward Center of Gravity SE" = "Spring Smallcopeall Eastward Center of Gravity SE",
                                      "Spring Smcopeall Northward Center of Gravity" = "Spring Smallcopeall Northward Center of Gravity",
                                      "Spring Smcopeall Northward Center of Gravity SE" = "Spring Smallcopeall Northward Center of Gravity SE"))

  zooplankton_index<- rbind(fallcalfin, springcalfin, fallcalfincog, springcalfincog,
                            falleuph, springeuph, falleuphcog, springeuphcog, fallsmallcope,
                            springsmallcope, fallsmallcopecog, springsmallcopecog, falllgcope,
                            springlgcope, falllgcopecog, springlgcopecog, fallzoopvol,
                            springzoopvol, fallzoopvolcog, springzoopvolcog, fallsmallcopeALL,
                            springsmallcopeALL, fallsmallcopeALLcog, springsmallcopeALLcog) |>
    tibble::as_tibble()

  # Index should have NA for missing surveys in Fall 2020,  not 0
  zooplankton_index$Value[zooplankton_index$Time==2020 & zooplankton_index$Value==0] <- NA


  if (save_clean){
    usethis::use_data(zooplankton_index, overwrite = T)
  } else {
    return(zooplankton_index)
  }
}
get_zooplankton_index(save_clean = T)
