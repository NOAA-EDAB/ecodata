#

library(dplyr)
library(tidyr)
library(readxl)
raw.dir <- here::here("data-raw")
aquaculture_xlsx <- "aqauculture2.xlsx"

get_aquaculture <- function(save_clean = F){
  dat <- read_excel(file.path(raw.dir,aquaculture_xlsx))

  ne_aquaculture<- dat %>%
    mutate(Pieces = as.numeric(Pieces)) %>%
    tidyr::pivot_longer(cols = c(Pieces,`Shellfish lease Acres`, `Production/Acre`),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::rename(Time = Year,
                  Region = State) %>%
    dplyr::mutate(Region = dplyr::recode(Region, "Maine" = "ME",
                                         "NEwHampshire" = "NH",
                                         "Mass" = "MA",
                                         "RhodeIsland" = "RI"))

  dat2 <- read_excel(file.path(raw.dir,aquaculture_xlsx), sheet = "Mid Atlantic")

  mab_aquaculture<- dat2  %>%
    dplyr::rename(Time = Year,
                  Region = State,
                  Value = Pieces) %>%
    dplyr::mutate(Var = c("Pieces"))

  aquaculture <- rbind(ne_aquaculture, mab_aquaculture)
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
