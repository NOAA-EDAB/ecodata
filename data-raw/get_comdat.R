# Process commerical data


# These data were derived from the Commercial Fisheries Database Biological Sample.
# More information about these data are available at
# https://noaa-edab.github.io/tech-doc/comdat.html

library(dplyr)


raw.dir <- here::here("data-raw")

get_comdat <- function(save_clean = F){

  load(file.path(raw.dir, "Commerical_data_pull_20.RData"))

  comdat <- commercial %>%
    dplyr::rename(EPU = Region) %>%
    dplyr::select(-Source)

  if (save_clean){
    usethis::use_data(comdat, overwrite = T)
  } else {
    return(comdat)
  }

}
get_comdat(save_clean = T)


