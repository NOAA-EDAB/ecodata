library(dplyr)
library(tidyr)

#Get raw
raw.dir <- here::here("data-raw") #input raw
seabird_mab_csv <- "mab_bird.csv"
get_seabird_mab <- function(save_clean = F){

  d <- read.csv(file.path(raw.dir,seabird_mab_csv ))

  seabird_mab <- d %>%
    dplyr::mutate(EPU = c("MAB"),
           Units = c("Number of breeding pairs"))

  if (save_clean){
    usethis::use_data(seabird_mab, overwrite = T)
  } else {
    return(seabird_mab)
  }
  # metadata ----
  attr(seabird_mab, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/ma-waterbird-productivity.html"
  attr(seabird_mab, "data_files")   <- list(
    seabird_mab_csv  = seabird_mab_csv )
  attr(seabird_mab, "data_steward") <- c(
    "Zachary Loman <zachary.loman@maine.edu >")
}
get_seabird_mab(save_clean = T)

