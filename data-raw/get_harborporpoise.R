# Process harbor porpoise bycatch estimates

# Time series figure is 5-yr running mean for harbor porpoise bycatch estimates
# for the Northeast US across all fisheries.

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
# harborporpoise_csv<-"1994-2019_5yr_hp_est.xlsx"
# #HP bycatch time series estimates------------------------------------------------------
# get_harborporpoise <- function(save_clean = F){
#   d <- readxl::read_excel(file.path(raw.dir,harborporpoise_csv)) %>%
#     dplyr::mutate(Region = "All")#
#
#   harborporpoise <- d %>%
#     tidyr::pivot_longer(cols = c("EST", "CV", "PBR", "LCI", "UCI"), names_to = "Var", values_to = "Value") %>%
#     mutate(Units = "N") %>%
#     as.data.frame()
#
#   if (save_clean){
#     usethis::use_data(harborporpoise, overwrite = TRUE)
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
# get_harborporpoise(save_clean = T)



#-------------------------------------------------------------------------------------------------------------------------------


harborporpoise_csv<-"historical_harbp_est.csv"



get_harborporpoise <- function(save_clean = F){
  # end of parameters

  historyEstCV <- read.csv(file.path(raw.dir,harborporpoise_csv), header=TRUE, stringsAsFactors=FALSE)

  rollwindw <- 5
  # find the columns that contain estimates and CVs
  estimateCols <- grep("est", colnames(historyEstCV), ignore.case=T)
  cvCols <- grep("cv", colnames(historyEstCV), ignore.case=T)

  nyrs <- nrow(historyEstCV)   # number of years available in the data

  library("boot")

  # prepare to calculate rolling means and upper & lower confidence intervals for the estimates
  history <- data.frame(year=historyEstCV$YEAR, pbr=historyEstCV$PBR,
                        NEgillest1y=historyEstCV$NE.Gillnet.1.YR.EST,
                        totalest1y=apply(historyEstCV[, estimateCols], 1, sum, na.rm=T),
                        NEgill1yLCI=NA, NEgill1yUCI=NA, total1yLCI=NA, total1yUCI=NA,
                        NEgillest5y=NA, totalest5y=NA, NEgill5yLCI=NA, NEgill5yUCI=NA,
                        total5yLCI=NA, total5yUCI=NA)
  history[history == 0] <- NA
  history$NEgillest5y <- c(rep(NA, rollwindw-1), apply(embed(history$NEgillest1y, rollwindw), 1, mean, na.rm=T))
  history$totalest5y <- c(rep(NA, rollwindw-1), apply(embed(history$totalest1y, rollwindw), 1, mean, na.rm=T))

  # assume lognormal distribution, which may not be right but may be the best we can do

  getCVofsum <- function(submeans, cvs) {
    vars <- sum((submeans * cvs)^2, na.rm=T)
    return(sqrt(vars) / sum(submeans, na.rm=T))
  }

  getCVofmean <- function(means, cvs, omitzerovar=1) {
    v <- sum((means * cvs)^2, na.rm=T)  # sum of individual variances
    vars <- ifelse(omitzerovar, v / sum((means * cvs) != 0, na.rm=T)^2, v / length(means)^2)
    return(sqrt(vars) / mean(means, na.rm=T))
  }

  # prepare to calculate rolling means and upper & lower confidence intervals for the estimates
  history <- data.frame(year=historyEstCV$YEAR, pbr=historyEstCV$PBR,
                        NEgillest1y=historyEstCV$NE.Gillnet.1.YR.EST,
                        NEgillcv1y=historyEstCV$NE.Gillnet.1.YR.CV,
                        totalest1y=apply(historyEstCV[, estimateCols], 1, sum, na.rm=T),
                        totalcv1y=NA,
                        NEgill1yLCI=NA, NEgill1yUCI=NA, total1yLCI=NA, total1yUCI=NA,
                        NEgillest5y=NA, NEgillcv5y=NA, totalest5y=NA, totalcv5y=NA,
                        NEgill5yLCI=NA, NEgill5yUCI=NA,
                        total5yLCI=NA, total5yUCI=NA)
  history[history == 0] <- NA
  history$NEgillest5y <- c(rep(NA, rollwindw-1), apply(embed(history$NEgillest1y, rollwindw), 1, mean, na.rm=T))
  history$totalest5y <- c(rep(NA, rollwindw-1), apply(embed(history$totalest1y, rollwindw), 1, mean, na.rm=T))
  for (i in seq_len(nrow(history))) {
    history$totalcv1y[i] <- getCVofsum(unlist(historyEstCV[i, estimateCols]),
                                       unlist(historyEstCV[i, cvCols]))
    if (i >= rollwindw) {
      j <- i - rollwindw + 1
      history$NEgillcv5y[i] <- getCVofmean(history$NEgillest1y[j:i],
                                           history$NEgillcv1y[j:i], omitzerovar=1)
      history$totalcv5y[i] <- getCVofmean(history$totalest1y[j:i],
                                          history$totalcv1y[j:i], omitzerovar=1)
    }
  }

  # calculate CI assuming lognormal distribution
  quant <- qnorm(.975)
  history$NEgill1yLCI <- history$NEgillest1y / exp(quant*sqrt(log(1+history$NEgillcv1y^2)))
  history$NEgill1yUCI <- history$NEgillest1y * exp(quant*sqrt(log(1+history$NEgillcv1y^2)))
  history$NEgill5yLCI <- history$NEgillest5y / exp(quant*sqrt(log(1+history$NEgillcv5y^2)))
  history$NEgill5yUCI <- history$NEgillest5y * exp(quant*sqrt(log(1+history$NEgillcv5y^2)))
  history$total1yLCI <- history$totalest1y / exp(quant*sqrt(log(1+history$totalcv1y^2)))
  history$total1yUCI <- history$totalest1y * exp(quant*sqrt(log(1+history$totalcv1y^2)))
  history$total5yLCI <- history$totalest5y / exp(quant*sqrt(log(1+history$totalcv5y^2)))
  history$total5yUCI <- history$totalest5y * exp(quant*sqrt(log(1+history$totalcv5y^2)))


  harborporpoise <- history %>%
    dplyr::select(year, pbr, totalest5y, total5yUCI, total5yLCI, totalest1y)%>%
    tidyr::pivot_longer(cols = !c(year),  names_to = "Var", values_to = "Value" ) %>%
    dplyr::mutate(Time = year,
                  EPU = c("ALL")) %>%
    tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU)

  # metadata ----
  attr(harborporpoise, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/harbor-porpoise-bycatch.html"
  attr(harborporpoise, "data_files")   <- list(
    harborporpoise_csv = harborporpoise_csv)
  attr(harborporpoise, "data_steward") <- c(
    "Chris Orphanides <chris.orphanides@noaa.gov>")
  attr(harborporpoise, "plot_script") <- list(
    `mf_MAB` = "macrofauna_MAB.Rmd-harborporpoise.R")

  if (save_clean){
    usethis::use_data(harborporpoise, overwrite = TRUE)
  } else {
    return(harborporpoise)
  }
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
