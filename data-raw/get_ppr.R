### Primary Production Required - Andy Beet and Mike Fogarty

library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")
ppr_rds<-"PPR_2020.rds"
get_ppr <- function(save_clean = F){

  ppr<-readRDS(file.path(raw.dir, ppr_rds)) %>%
    dplyr::rename(Time  = Year)%>%
    dplyr::select(Time, Var, Value, EPU, Units)

  # metadata ----
  attr(ppr, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/ecosystem-overfishing.html"
  attr(ppr, "data_files")   <- list(
    ppr_rds = ppr_rds)
  attr(ppr, "data_steward") <- c(
    "Andrew Beet <andrew.beet@noaa.gov>")
  attr(ppr, "plot_script") <- list(
    `hd_MAB` = "human_dimensions_MAB.Rmd-ppr.R",
    `hd_NE` = "human_dimensions_NE.Rmd-ppr.R",
    `hd_NE_comp90` = "human_dimensions_NE.Rmd-ppr_comp90.R",
    `hd_NE_index` = "human_dimensions_NE.Rmd-ppr_index.R",
    `hd_NE_mtl` = "human_dimensions_NE.Rmd-ppr_mtl.R")

  if (save_clean){
    usethis::use_data(ppr, overwrite = T)
  } else {
    return(ppr)
  }
}
get_ppr(save_clean = T)

