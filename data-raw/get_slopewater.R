# Process slopewater proportion time series
#
# Slopewater proportions give the percent total of water type observed in
# the deep Northeast Channel (150-200 m depth).
#
# Raw data fields correspond to year, water mass flavor (WSW = Warm Slope Water, LSLW = Labrador Slope Water),
# and proportion of total expressed as a percentage.


library(dplyr)
library(tidyr)

#Get raw
raw.dir <- here::here("data-raw") #input raw
slopewater_csv <- "slopewater_proportions.csv"
get_slopewater <- function(save_clean = F){

  d <- read.csv(file.path(raw.dir,slopewater_csv))

  slopewater <- d %>%
    dplyr::rename(Time = year, Var = water.mass.flavor, Value = prop) %>%
    dplyr::mutate(EPU = "GOM", Units = "unitless", Var2 = "proportion ne channel") %>%
    tidyr::unite(.,Var,c(Var,Var2), sep = " ") %>%
    as.data.frame() %>% tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  # metadata ----
  attr(slopewater, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/slopewater-proportions.html"
  attr(slopewater, "data_files")   <- list(
    slopewater_csv = slopewater_csv)
  attr(slopewater, "data_steward") <- c(
    "Paula Frantantoni <paula.fratantoni@noaa.gov>")
  attr(slopewater, "plot_script") <- list(
    `ltl_NE` = "LTL_NE.Rmd-slopewater.R")

  if (save_clean){
    usethis::use_data(slopewater, overwrite = T)
  } else {
    return(slopewater)
  }
}
get_slopewater(save_clean = T)
