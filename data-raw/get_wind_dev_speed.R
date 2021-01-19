## Wind develop areas

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

raw.dir <- here::here("data-raw")
wind_dev_speed_xlsx<- "Silva_Data_Wind_Speed_Extent - Angela Silva - NOAA Affiliate.xlsx"

get_wind_dev_speed <- function(save_clean = F){
  wind_dev_speed <- read_excel(file.path(raw.dir,wind_dev_speed_xlsx)) %>%
    dplyr::rename(Time = Constr_Yrs,
                  Cumulative_area = Cumul_Proj_Acres,
                  Cumulative_foundations = Cumul_FDNS,
                  Cumulative_seafloor_disturbance = "Cumul_ SeaDist_FndScr_Acres") %>%
    dplyr::select(Project_Name, Time, Cumulative_seafloor_disturbance, Cumulative_area, Cumulative_foundations) %>%
    tidyr::pivot_longer(cols = starts_with("Cumulative"),  names_to = "Var", values_to = "Value")

  if (save_clean){
    usethis::use_data(wind_dev_speed, overwrite = T)
  } else {
    return(wind_dev_speed)
  }
  # metadata ----
  attr(wind_dev_speed, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc.html"
  attr(wind_dev_speed, "data_files")   <- list(
    wind_dev_speed_xlsx = wind_dev_speed_xlsx)
  attr(wind_dev_speed, "data_steward") <- c(
    "Angela Silva <angela.silva@noaa.gov")
}
get_wind_dev_speed(save_clean = T)
