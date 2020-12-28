#

library(dplyr)
library(tidyr)
library(readxl)
raw.dir <- here::here("data-raw")
aquaculture_xlsx <- "Aquaculture.edited.xlsx"

get_aquaculture <- function(save_clean = F){
  dat <- read_excel(file.path(raw.dir,aquaculture_xlsx))

  aquaculture<- dat %>%
    tidyr::pivot_longer(cols = c(Pieces,`Shellfish lease Acres`, `Production/Acre`),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::rename(Time = Year,
                  Region = State)


  if (save_clean){
    usethis::use_data(aquaculture, overwrite = T)
  } else {
    return(aquaculture)
  }
  # metadata ----
  attr(aquaculture, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/aquaculture.html"
  attr(aquaculture, "data_files")   <- list(
    aquaculture_xlsx = aquaculture_xlsx)
  attr(aquaculture, "data_steward") <- c(
    "Chris Schillaci <christopher.shillaci@noaa.gov>")
}
get_aquaculture(save_clean = T)
