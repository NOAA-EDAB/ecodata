library(dplyr)
library(stringr)
library(tidyr)

raw.dir <- here::here("data-raw")
effective_sweptarea_rdata <- "ATT87886.RData"

get_effective_sweptarea <- function(save_clean = F){

  temp_env = environment()
  load(file.path(raw.dir, effective_sweptarea_rdata),envir = temp_env)

  assign("effective_sweptarea", temp_env$final_sweptarea, envir = .GlobalEnv)

  # 5. Remove the temporary environment to clean up memory
  rm(temp_env)


  if (save_clean){
    usethis::use_data(effective_sweptarea, overwrite = T)
  } else {
    return(effective_sweptarea)
  }
}
get_effective_sweptarea(save_clean = T)
