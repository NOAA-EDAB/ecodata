library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

raw.dir <- here::here("data-raw")
zoo_comm_file = "Morse_20251216_PC_zoo - Ryan Morse - NOAA Affiliate.rdata"
### Zooplankton Diversity
get_zoo_community <- function(save_clean = F){

  #submitted as Rdata. Read into temp env then write new name
  temp.env <- new.env()
  data <- load(file.path(raw.dir,zoo_comm_file), envir = temp.env)
  zoo_community <- (temp.env[[data[1]]])%>%
    dplyr::rename(Time = year,
                  EPU = epu) %>%
    mutate(Value  = as.numeric(Value)) %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  if (save_clean){
    usethis::use_data(zoo_community, overwrite = T)
  } else {
    return(zoo_community)
  }
  # metadata ----
  attr(zoo_community, "tech-doc_url") <- ""
  attr(zoo_community, "data_files")   <- list(
    zoo_comm_file = zoo_comm_file)
  attr(zoo_community, "data_steward") <- c(
    "Ryan Morse <ryan.morse@noaa.gov>")
}
get_zoo_community(save_clean = T)



