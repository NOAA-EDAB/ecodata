#Processing code for oyster harvest data in the Mid-Atlantic

#These data were collected directly from shellfish aquaculture surveys performed by state agencies and
#universities.

#NJ: http://njseagrant.org/new-jersey-shellfish-aquaculture-situation-outlook-report-new/
#VA: http://www.vims.edu/research/units/centerspartners/map/aquaculture/docs_aqua/vims_mrr_2018-9.pdf
#MD: Data from MD Aquaculture Coordinating Council meeting reports. Newest reports available upon request

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")

get_aquaculture <- function(save_clean = F){
  aquaculture <- read.csv(file.path(raw.dir,"mab_oyster_harvest.csv"))

  if (save_clean){
    usethis::use_data(aquaculture, overwrite = T)
  } else {
    return(aquaculture)
  }

}
