#Processing code for oyster harvest data in the Mid-Atlantic

#These data were collected directly from shellfish aquaculture surveys
# performed by state agencies and universities.

#NJ: http://njseagrant.org/new-jersey-shellfish-aquaculture-situation-outlook-report-new/
#VA: http://www.vims.edu/research/units/centerspartners/map/aquaculture/docs_aqua/vims_mrr_2018-9.pdf
#MD: Data from MD Aquaculture Coordinating Council meeting reports.
# Newest reports available upon request

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
aquaculture_csv <- "mab_oyster_harvest.csv"

get_aquaculture <- function(save_clean = F){
  aquaculture <- read.csv(file.path(raw.dir,aquaculture_csv))

  if (save_clean){
    usethis::use_data(aquaculture, overwrite = T)
  } else {
    return(aquaculture)
  }
  # metadata ----
  attr(aquaculture, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/aquaculture.html"
  attr(aquaculture, "data_files")   <- list(
    aquaculture_csv = aquaculture_csv)
  attr(aquaculture, "data_steward") <- c(
    "Kimberly Bastille <kimberly.bastille@noaa.gov>")
}
get_aquaculture(save_clean = T)
