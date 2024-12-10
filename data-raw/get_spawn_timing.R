## Spawn timing
raw.dir<- here::here("data-raw/")
sptime <- "spawn_timing - Sarah Gaichas - NOAA Federal.rds"

get_spawn_timing <- function(save_clean = F){

  spawn_timing<- readRDS(file.path(raw.dir, sptime))

  if (save_clean){
    usethis::use_data(spawn_timing, overwrite = T)
  } else {
    return(spawn_timing)
  }
}
get_spawn_timing(save_clean = T)
