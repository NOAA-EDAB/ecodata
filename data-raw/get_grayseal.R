# Process Gray Seal bycatch estimates

# Time series figure is 5-yr running mean for Gray Sealbycatch estimates
# for the Northeast US across all fisheries.

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
# grayseal_csv<-"1996-2019_5yr_gs_est_wPBR.xlsx"
# #HP bycatch time series estimates------------------------------------------------------
# get_grayseal <- function(save_clean = F){
#   d <- readxl::read_excel(file.path(raw.dir,grayseal_csv)) %>%
#     dplyr::mutate(Region = "All") %>%
#     dplyr::rename(EST5 = `5-YR EST`,
#                   EST1 = `NE 1-YR EST`) %>%
#     dplyr::select(-...8,
#                   -LCI_DIFF,
#                   -UCI_DIFF,
#                   -...11,
#                   -...12) #%>%
#     #dplyr::group_by(Year, Region) %>%
#     #tidyr::pivot_wider(names_from = Var, values_from = Value)
#
#   #Create confidence intervals
#   # var1nnum <- log(1+d$CV^2)
#   # c <- exp(1.96 * sqrt(var1nnum))
#   # d$up95ci <- d$EST * c
#   # d$lo95ci <- d$EST / c
#
#
#   grayseal <- d %>%
#     tidyr::pivot_longer(cols = c("EST5", "EST1","CV", "PBR", "LCI", "UCI"), names_to = "Var", values_to = "Value") %>%
#     mutate(Units = "N") #%>%
#     #as.data.frame()
#
#   if (save_clean){
#     usethis::use_data(grayseal, overwrite = TRUE)
#   } else {
#     return(grayseal)
#   }
#   # metadata ----
#   attr(grayseal, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/harbor-porpoise-bycatch.html"
#   attr(grayseal, "data_files")   <- list(
#     grayseal_csv = grayseal_csv)
#   attr(grayseal, "data_steward") <- c(
#     "Chris Orphanides <chris.orphanides@noaa.gov>")
# }
# get_grayseal(save_clean = T)

#---------------------------------------------------------------------------------------------

grayseal_csv<-"historical_gray_est.csv"



get_grayseal <- function(save_clean = F){
  # end of parameters

  historyEstCV <- read.csv(file.path(raw.dir,grayseal_csv), header=TRUE, stringsAsFactors=FALSE)

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


  grayseal <- history %>%
    dplyr::select(year, pbr, totalest5y, total5yUCI, total5yLCI, totalest1y)%>%
    tidyr::pivot_longer(cols = !c(year),  names_to = "Var", values_to = "Value" ) %>%
    dplyr::mutate(Time = year,
                  EPU = c("ALL")) %>%
    tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU)

  gs20_21<- data.frame(Time = c(2021, 2021, 2021, 2021, 2021,
                                2020, 2020, 2020, 2020, 2020),
                       Var = c("pbr","totalest5y", "total5yUCI", "total5yLCI", "totalest1y",
                               "pbr","totalest5y", "total5yUCI", "total5yLCI", "totalest1y"),
                       Value = c(1389, 1351, 1577, 1158, 109,
                                 1389, 1248, 1420, 1097, 1418),
                       EPU = c("ALL"))
  grayseal<- grayseal %>% rbind(gs20_21)
  # metadata ----
  attr(grayseal, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/harbor-porpoise-and-gray-seal-bycatch.html"
  attr(grayseal, "data_files")   <- list(
    grayseal_csv = grayseal_csv)
  attr(grayseal, "data_steward") <- c(
    "Chris Orphanides <chris.orphanides@noaa.gov>")
  attr(grayseal, "plot_script") <- list(
    `mf_MAB` = "macrofauna_MAB.Rmd-grayseal.R",
    `mf_MAB_mab` = "macrofauna_MAB.Rmd-grayseal-mab.R")

  if (save_clean){
    usethis::use_data(grayseal, overwrite = TRUE)
  } else {
    return(grayseal)
  }
}
get_grayseal(save_clean = T)
