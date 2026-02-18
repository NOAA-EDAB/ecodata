# Process productivity_anomaly indicator
raw.dir <- here::here('data-raw')
productivity_anomaly_input <- "productivity_anomaly.rds"

get_productivity_anomaly <- function(save_clean = F) {
  productivity_anomaly <- readRDS(file.path(
    raw.dir,
    productivity_anomaly_input
  ))

  if (save_clean) {
    usethis::use_data(productivity_anomaly, overwrite = T)
  } else {
    return(productivity_anomaly)
  }
}
get_productivity_anomaly(save_clean = T)
