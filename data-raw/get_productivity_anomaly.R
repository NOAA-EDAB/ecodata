# Process productivity_anomaly indicator
raw.dir <- here::here('data-raw')
productivity_anomaly_input <- "productivity_anomaly.rds"

get_productivity_anomaly <- function(save_clean = F) {
  productivity_anomaly <- readRDS(file.path(
    raw.dir,
    productivity_anomaly_input
  ))

  # Remove Tilefish (all variants) from Mid-Atlantic report per reviewer request (03/16/2026)
  # Excludes: TILEFISH_Survey, NE LME TILEFISH_Survey, Tilefish -...-Assessment

  productivity_anomaly <- productivity_anomaly[
    !grepl("tilefish", productivity_anomaly$Var, ignore.case = TRUE),
  ]

  if (save_clean) {
    usethis::use_data(productivity_anomaly, overwrite = T)
  } else {
    return(productivity_anomaly)
  }
}
get_productivity_anomaly(save_clean = T)
