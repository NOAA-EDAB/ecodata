# Process commerical data


# These data were derived from the Commercial Fisheries Database Biological Sample.
# More information about these data are available at
# https://noaa-edab.github.io/tech-doc/comdat.html

library(dplyr)


raw.dir <- here::here("data-raw")

comdat_RData<-"Commercial_data_pull_23 (7).RData"

get_comdat <- function(save_clean = F){

  load(file.path(raw.dir, comdat_RData))

  comdat <- commercial %>%
    dplyr::rename(EPU = Region) %>%
    dplyr::select(-Source)%>%
    tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  # metadata ----
  attr(comdat, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/comdat.html"
  attr(comdat, "data_files")   <- list(
    comdat_RData = comdat_RData)
  attr(comdat, "data_steward") <- c(
    "Sean Lucey <sean.lucey@noaa.gov>")
  attr(comdat, "plot_script") <- list(
    `hd_MAB_comm-revenue` = "human_dimensions_MAB.Rmd-comdat-comm-revenue.R",
    `hd_MAB_commercial-landings` = "human_dimensions_MAB.Rmd-comdat-commercial-landings.R",
    `hd_MAB_total-landings` = "human_dimensions_MAB.Rmd-comdat-total-landings.R",
    `hd_NE_comm-revenue` = "human_dimensions_NE.Rmd-comdat-comm-revenue.R",
    `hd_NE_commercial-landings` = "human_dimensions_NE.Rmd-comdat-commercial-landings.R",
    `hd_NE_commercial-landings-gb` = "human_dimensions_NE.Rmd-comdat-commercial-landings-gb.R",
    `hd_NE_commercial-landings-gom` = "human_dimensions_NE.Rmd-comdat-commercial-landings-gom.R",
    `hd_NE_total-landings` = "human_dimensions_NE.Rmd-comdat-total-landings.R")

  if (save_clean){
    usethis::use_data(comdat, overwrite = T)
  } else {
    return(comdat)
  }
}
get_comdat(save_clean = T)












