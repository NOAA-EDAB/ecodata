# Process commerical data
# These data were derived from the Commercial Fisheries Database Biological Sample.
# More information about these data are available at
# https://noaa-edab.github.io/tech-doc/comdat.html

raw.dir <- here::here("data-raw")

comdat_input <- "comdat.rds"

get_comdat <- function(save_clean = F) {
  comdat <- readRDS(file.path(raw.dir, comdat_input))

  if (save_clean) {
    usethis::use_data(comdat, overwrite = T)
  } else {
    return(comdat)
  }
}
get_comdat(save_clean = T)
