# Hudson River Flow

library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")

hrf_csv<-"RiverFlow_OCT_23_2024 - Ian Maywar.csv"

get_hudson_river_flow <- function(save_clean = F){

  dat <- read.csv(file.path(raw.dir,hrf_csv), header=TRUE, stringsAsFactors=FALSE)

  hudson_river_flow <- dat %>%
    dplyr::select(-X, -Loc, -N) %>%
    dplyr::mutate(EPU = c("MAB"),
                  Units = c("cubic feet per second")) %>%
    dplyr::rename(Time = Year,
                  Value = Val,
                  Var = Variable)%>%
    tibble::as_tibble()

  # metadata ----
  attr(hudson_river_flow, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc.html"
  attr(hudson_river_flow, "data_files")   <- list(
    hrf_csv = hrf_csv )
  attr(hudson_river_flow, "data_steward") <- c(
    "Laura Gruenburg <laura.gruenburg@stonybrook.edu>")
  attr(hudson_river_flow, "plot_script") <- list()

  if (save_clean){
    usethis::use_data(hudson_river_flow, overwrite = TRUE)
  } else {
    return(hudson_river_flow)
  }
}
get_hudson_river_flow(save_clean = T)
