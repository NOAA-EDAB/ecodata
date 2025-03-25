# Processing for recreational fishing indicators

library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

get_rec <- function(save_clean = F){

  files = list.files(raw.dir, pattern = "REC_HARVEST_2025|Rec_angler_effort_2025|Rec_Species_Diversity_2025")
  for (i in 1:length(files)) assign(files[i], read.csv(file.path(raw.dir,files[i])))

  recdat <- NULL
  for (i in ls()){
    if (stringr::str_detect(i, "REC_|Rec_")){
      d <- get(i) %>%
        dplyr::select(Time, EPU = Region, Value, Units, Var)

      assign('recdat',rbind(recdat, d))


    }
  }

  recdat <- recdat %>% dplyr::filter(!is.na(EPU)) %>%
    tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  if (save_clean){
    usethis::use_data(recdat, overwrite = T)
  } else {
    return(recdat)
  }
  # metadata ----
  attr(recdat, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/recreational-fishing-indicators.html"
  attr(recdat, "data_files")   <- list()
  attr(recdat, "data_steward") <- c(
    "Geret DePiper <geret.depiper@noaa.gov>")
}
get_rec(save_clean = T)
