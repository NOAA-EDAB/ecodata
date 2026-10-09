# Process Bennet indicator; price and volume indicators
raw.dir <- here::here('data-raw')
bennet_input <- "bennet.rds"

get_bennet <- function(save_clean = F) {
  bennet <- readRDS(file.path(raw.dir, bennet_input))

  if (save_clean) {
    usethis::use_data(bennet, overwrite = T)
  } else {
    return(bennet)
  }
}
get_bennet(save_clean = T)
