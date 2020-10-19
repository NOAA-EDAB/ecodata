# Process Bennet indicator; price and volume indicators

library(dplyr)
library(tidyr)
library(magrittr)

raw.dir <- here::here('data-raw')
bennet_Rdata<- "Bennet_Index_20.Rdata"
get_bennet <- function(save_clean = F){

  load(file.path(raw.dir, bennet_Rdata))
  bennet <- bennet %>%
    dplyr::rename(EPU = Region)

  if (save_clean){
    usethis::use_data(bennet, overwrite = T)
  } else {
    return(bennet)
  }
  # metadata ----
  attr(bennet, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/bennet-indicator.html"
  attr(bennet, "data_files")   <- list(
    bennet_Rdata = bennet_Rdata)
  attr(bennet, "data_steward") <- c(
    "John Walden <john.walden@noaa.gov>",
    "Geret DePiper <geret.depiper@noaa.gov>")
}
get_bennet(save_clean = T)
