### Primary Production Required - Andy Beet and Mike Fogarty

library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")
ppr_rds<-"PPR.rds"
get_ppr <- function(save_clean = F){

  ppr<-readRDS(file.path(raw.dir, ppr_rds)) %>%
    dplyr::rename(Time  = YEAR,
           EPU = REGION,
           Value = INDEX) %>%
    dplyr::mutate(Units = c("%"),
           Var = c("Primary Production Required"))

  if (save_clean){
    usethis::use_data(ppr, overwrite = T)
  } else {
    return(ppr)
  }
  # metadata ----
  attr(ppr, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/primary-production-required.html"
  attr(ppr, "data_files")   <- list(
    ppr_rds = ppr_rds)
  attr(ppr, "data_steward") <- c(
    "Andrew Beet <andrew.beet@noaa.gov>")
}
get_ppr(save_clean = T)
