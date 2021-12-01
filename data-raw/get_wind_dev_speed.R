## Wind develop areas

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

raw.dir <- here::here("data-raw")
wind_dev_speed_xlsx<- "Tables_Cumulative Totals by Construction Year_01_20_21.xlsx"

get_wind_dev_speed <- function(save_clean = F){
  wind_dev_speed <- read_excel(file.path(raw.dir,wind_dev_speed_xlsx), sheet = 3) %>%
    janitor::row_to_names(row_number = 1) %>%
    dplyr::rename(Time = "Year End",
                  Value = "Project Acres") %>%
    dplyr::mutate( Var = c("WindDevelopment"),
                  EPU = c("ALL"))%>%
    dplyr::select( Time, Value, Var, EPU) %>%
    dplyr::filter(!Value == "NA")

  # metadata ----
  attr(wind_dev_speed, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc.html"
  attr(wind_dev_speed, "data_files")   <- list(
    wind_dev_speed_xlsx = wind_dev_speed_xlsx)
  attr(wind_dev_speed, "data_steward") <- c(
    "Angela Silva <angela.silva@noaa.gov")
  attr(wind_dev_speed, "plot_script") <- list(
    `hd_MAB_0` = "human_dimensions_MAB.Rmd-wind-dev-speed0.R",
    `hd_MAB_0.30` = "human_dimensions_MAB.Rmd-wind-dev-speed0.30.R")

  if (save_clean){
    usethis::use_data(wind_dev_speed, overwrite = T)
  } else {
    return(wind_dev_speed)
  }
}
get_wind_dev_speed(save_clean = T)











raw.dir <- here::here("data-raw")
wind_dev_speed_csv<- "Wind_Cumul_Develop_2021-22 - Angela Silva - NOAA Affiliate.csv"

get_wind_dev_speed <- function(save_clean = F){
  wind_dev_speed <- read.csv(file.path(raw.dir,wind_dev_speed_csv)) %>%
    dplyr::filter(!Constr_Years == "Construction is estimated to be 2 years for each project.") %>%
    tidyr::pivot_longer(cols = c(  "Cumul_Proj_Acres" , "Cumul_.FDNS" , "Cumul_Offsh_Cbl__Acres",
                                   "Cumul_OffExp_Inter_Cab_Miles", "TBNSinstall_No"),
                        names_to = "Var", values_to = "Value") %>%
    tidyr::separate(Constr_Years, into = c("Time", "Location"), sep = "[()]") %>%
    dplyr::mutate(Var = paste0(Var, "::", Location),
                  Time = as.numeric(Time),
                  Value = as.numeric(gsub(",","",Value)),
                  EPU = c("All")) %>%
    dplyr::select(!Location) %>%
    dplyr::filter(!Value == "NA")

  # metadata ----
  attr(wind_dev_speed, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc.html"
  attr(wind_dev_speed, "data_files")   <- list(
    wind_dev_speed_xlsx = wind_dev_speed_xlsx)
  attr(wind_dev_speed, "data_steward") <- c(
    "Angela Silva <angela.silva@noaa.gov")
  attr(wind_dev_speed, "plot_script") <- list(
    `hd_MAB_0` = "human_dimensions_MAB.Rmd-wind-dev-speed0.R",
    `hd_MAB_0.30` = "human_dimensions_MAB.Rmd-wind-dev-speed0.30.R")

  if (save_clean){
    usethis::use_data(wind_dev_speed, overwrite = T)
  } else {
    return(wind_dev_speed)
  }
}
get_wind_dev_speed(save_clean = T)
