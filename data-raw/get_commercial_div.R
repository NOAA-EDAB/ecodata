# Process commercial diversity data

# More information about these data are available at
# https://noaa-edab.github.io/tech-doc/catch-and-fleet-diversity.html.
# Data are drawn from blend of VTR trip data, CFDBS prices,
# vessel characteristics from PERMIT databases,
# and major VTR gear by permit. Here "MA" and "NE" refer to the
# Mid-Atlantic and New England regions respectively, and are
# not derived by EPU.

library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")
commercial_div_csv <- "Commercial_Diversity_2025 - Geret DePiper - NOAA Federal.csv"

get_commercial_div <- function(save_clean = F){
  commercial_div <- read.csv(file.path(raw.dir, commercial_div_csv)) %>%
    dplyr::select(-X, -Source) %>%
    dplyr::rename(EPU = Region) %>%
    as.data.frame()%>%
    tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU, Units) %>%
    dplyr::filter(Time >2002)

  commercial_div$Var <- stringr::str_replace(commercial_div$Var, "diveristy", "diversity")

  # metadata ----
  attr(commercial_div, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc"
  attr(commercial_div, "data_files")   <- list(
    commercial_div_csv = commercial_div_csv)
  attr(commercial_div, "data_steward") <- c(
    "Geret DePiper <geret.depiper@noaa.gov>")
  attr(commercial_div, "plot_script") <- list(
    `hd_MAB` = "human_dimensions_MAB.Rmd-commercial-div.R",
    `hd_MAB_species-div` = "human_dimensions_MAB.Rmd-commercial-div-species-div.R",
    `hd_NE` = "human_dimensions_NE.Rmd-commercial-div.R",
    `hd_NE_species-div` = "human_dimensions_NE.Rmd-commercial-div-species-div.R")

  if(save_clean){
    usethis::use_data(commercial_div, overwrite = T)
  } else {
    return(commercial_div)
  }
}
get_commercial_div(save_clean = T)



