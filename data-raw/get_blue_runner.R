### SAFMC managed species presence on the NES

## https://noaa-edab.github.io/tech-doc/safmc-managed-spp.html

library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")
blue_runner_RData <- "Blue_runner_presence.RData"
get_blue_runner <- function(save_clean = F){

   load(file.path(raw.dir, blue_runner_RData))

   blue_runner<- blue.soe %>%
     dplyr::rename(EPU =  Region)%>%
     tibble::as_tibble() %>%
     dplyr::select(Time, Var, Value, EPU, Units)

   # metadata ----
   attr(blue_runner, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/chl-pp.html"
   attr(blue_runner, "data_files")   <- list(
     blue_runner_RData = blue_runner_RData)
   attr(blue_runner, "data_steward") <- c(
     "Sean Lucey <sean.lucey@noaa.gov>")
   attr(blue_runner, "plot_script") <- list(
      `mf_MAB` = "macrofauna_MAB.Rmd-blue-runner.R")

   if (save_clean){
      usethis::use_data(blue_runner, overwrite = T)
   } else {
      return(blue_runner)
   }
}
get_blue_runner(save_clean = T)
