# Process commerical data


# These data were derived from the Commercial Fisheries Database Biological Sample.
# More information about these data are available at
# https://noaa-edab.github.io/tech-doc/comdat.html

library(dplyr)


raw.dir <- here::here("data-raw")

comdat_RData<-"Commerical_data_pull_20.RData"

get_comdat <- function(save_clean = F){

  load(file.path(raw.dir, comdat_RData))

  comdat <- commercial %>%
    dplyr::rename(EPU = Region) %>%
    dplyr::select(-Source)

  if (save_clean){
    usethis::use_data(comdat, overwrite = T)
  } else {
    return(comdat)
  }
  # metadata ----
  attr(comdat, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/comdat.html"
  attr(comdat, "data_files")   <- list(
    comdat_RData = comdat_RData)
  attr(comdat, "data_steward") <- c(
    "Sean Lucey <sean.lucey@noaa.gov>")
}
get_comdat(save_clean = T)


