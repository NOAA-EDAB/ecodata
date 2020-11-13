### Cold pool index from Zhuomin Chen

library(dplyr)
library(tidyr)
library(tidync)



raw.dir <- here::here("data-raw")

cold_pool_nc<- "Glorys12v1_ColdPool_Extents.nc"

get_cold_pool <- function(save_clean = F){


  cold_pool_file <- list.files(raw.dir, pattern=cold_pool_nc,full.names=TRUE)


  cold_pool <- tidync::tidync(cold_pool_file) %>%
    tidync::hyper_tibble() %>%
    dplyr::mutate(Time = c(ny+1992)) %>%
    dplyr::filter(V_max < 100,
                  T_peak < 100,
                  T_min < 100,
                  T_mean < 100) %>%
    tidyr::pivot_longer(cols = c("T_peak", "T_min", "T_mean", "V_max"), names_to = "Var",values_to = "Value") %>%
    dplyr::group_by(Time, Var) %>%
    dplyr::summarise(Val = mean(Value),
                     Uncertainty = sd(Value)) %>%
    dplyr::mutate(EPU = c("MAB"))


  if(save_clean){
    usethis::use_data(cold_pool, overwrite = T)
  } else {
    return(cold_pool)
  }
  # metadata ----
  attr(cold_pool, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/cold-pool-index.html"
  attr(cold_pool, "data_files")   <- list(
    cold_pool_nc = cold_pool_nc)
  attr(cold_pool, "data_steward") <- c(
    "Zhuomin Chen <zchen@whoi.edu>")

}
get_cold_pool(save_clean = T)

### Cold pool index from Chris Melrose
# cold_pool_csv<- "cold_pool_index.csv"
#
# get_cold_pool <- function(save_clean = F){
#
#   cold_pool <- read.csv(file.path(raw.dir, cold_pool_csv)) %>%
#     dplyr::rename(EPU = Region,
#                   Time = Year,
#                   Value = VAR)
#
#   if(save_clean){
#     usethis::use_data(cold_pool, overwrite = T)
#   } else {
#     return(cold_pool)
#   }
#   # metadata ----
#   attr(cold_pool, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/cold-pool-index.html"
#   attr(cold_pool, "data_files")   <- list(
#     cold_pool_csv = cold_pool_csv)
#   attr(cold_pool, "data_steward") <- c(
#     "Chris Melrose <chris.melrose@noaa.gov>")
#
# }
# get_cold_pool(save_clean = T)
