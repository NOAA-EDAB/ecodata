# Process Bennet indicator; price and volume indicators

library(dplyr)
library(tidyr)
library(magrittr)

raw.dir <- here::here('data-raw')
bennet_Rdata<- "Bennet_Index_21.Rdata"
get_bennet <- function(save_clean = F){

  load(file.path(raw.dir, bennet_Rdata))
  bennet <- bennet %>%
    dplyr::rename(EPU = Region)

  # metadata ----
  attr(bennet, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/bennet-indicator.html"
  attr(bennet, "data_files")   <- list(
    bennet_Rdata = bennet_Rdata)
  attr(bennet, "data_steward") <- c(
    "John Walden <john.walden@noaa.gov>",
    "Geret DePiper <geret.depiper@noaa.gov>")
  attr(bennet, "plot_script") <- list(
    `hd_MAB` = "human_dimensions_MAB.Rmd-bennet.R",
    `hd_MAB_2` = "human_dimensions_MAB.Rmd-bennet2.R",
    `hd_MAB_all` = "human_dimensions_MAB.Rmd-bennet-all.R",
    `hd_MAB_table` = "human_dimensions_MAB.Rmd-bennet-table.R",
    `hd_NE` = "human_dimensions_NE.Rmd-bennet.R",
    `hd_NE_2` = "human_dimensions_NE.Rmd-bennet2.R",
    `hd_NE_all` = "human_dimensions_NE.Rmd-bennet-all.R")

  if (save_clean){
    usethis::use_data(bennet, overwrite = T)
  } else {
    return(bennet)
  }
}
get_bennet(save_clean = T)
