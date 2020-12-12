# Process harbor porpoise bycatch estimates

# Time series figure is 5-yr running mean for harbor porpoise bycatch estimates
# for the Northeast US across all fisheries.

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
harborporpoise_csv<-"Orphanides_1994-2019_5yr_hp_est_LONG_FORMAT - Chris Orphanides - NOAA Federal.csv"
#HP bycatch time series estimates------------------------------------------------------
get_harborporpoise <- function(save_clean = F){
  d <- read.csv(file.path(raw.dir,harborporpoise_csv)) %>%
    dplyr::mutate(Region = "All") %>%
    dplyr::select(-X) %>%
    dplyr::group_by(Year, Region) %>%
    tidyr::pivot_wider(names_from = Var, values_from = Value)

  #Create confidence intervals
  var1nnum <- log(1+d$CV^2)
  c <- exp(1.96 * sqrt(var1nnum))
  d$up95ci <- d$EST * c
  d$lo95ci <- d$EST / c


  harborporpoise <- d %>%
    tidyr::pivot_longer(cols = c("EST", "CV", "PBR", "up95ci", "lo95ci"), names_to = "Var", values_to = "Value") %>%
    mutate(Units = "N") %>%
    as.data.frame()

  if (save_clean){
    usethis::use_data(harborporpoise, overwrite = TRUE)
  } else {
    return(harborporpoise)
  }
  # metadata ----
  attr(harborporpoise, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/harbor-porpoise-bycatch.html"
  attr(harborporpoise, "data_files")   <- list(
    harborporpoise_csv = harborporpoise_csv)
  attr(harborporpoise, "data_steward") <- c(
    "Chris Orphanides <chris.orphanides@noaa.gov>")
}
get_harborporpoise(save_clean = T)


#### Harborpoise bycatch from 2017 SOE
# harborporpoise_csv<-"1994-2017_5yr_hp_est.csv"
# #HP bycatch time series estimates------------------------------------------------------
# get_harborporpoise <- function(save_clean = F){
#   d <- read.csv(file.path(raw.dir,harborporpoise_csv))
#
#   #Create confidence intervals
#   var1nnum <- log(1+d$CV^2)
#   c <- exp(1.96 * sqrt(var1nnum))
#   d$up95ci <- d$EST * c
#   d$lo95ci <- d$EST / c
#
#
#   harborporpoise <- d %>% dplyr::rename(Time = YEAR) %>%
#     gather(., Var, Value, -Time) %>%
#     mutate(Units = "N",
#            EPU = "All",
#            Var, Var = plyr::mapvalues(Var, from = c("EST","CV","PBR","up95ci","lo95ci"),
#                                       to = c("harbor porpoise bycatch estimate",
#                                              "harbor porpoise bycatch cv",
#                                              "harbor porpoise bycatch pbr",
#                                              "harbor porpoise bycatch up95ci",
#                                              "harbor porpoise bycatch lo95ci"))) %>%
#     as.data.frame()
#
#   if (save_clean){
#     usethis::use_data(harborporpoise, overwrite = R)
#   } else {
#     return(harborporpoise)
#   }
#   # metadata ----
#   attr(harborporpoise, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/harbor-porpoise-bycatch.html"
#   attr(harborporpoise, "data_files")   <- list(
#     harborporpoise_csv = harborporpoise_csv)
#   attr(harborporpoise, "data_steward") <- c(
#     "Chris Orphanides <chris.orphanides@noaa.gov>")
# }
