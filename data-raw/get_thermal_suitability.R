## Fall thermal habitat suitability

### Documentation https://noaa-edab.github.io/tech-doc/thermal_suitability.html
### Input produced by READ-EDAB-enviroThresholds (R/05_export_soe.R)

raw.dir <- here::here("data-raw")
thermal_suitability_Rds <- "thermal_suitability.rds"

get_thermal_suitability <- function(save_clean = F) {
  thermal_suitability <- readRDS(file.path(raw.dir, thermal_suitability_Rds))

  if (save_clean) {
    usethis::use_data(thermal_suitability, overwrite = T)
  } else {
    return(thermal_suitability)
  }
}
get_thermal_suitability(save_clean = T)
