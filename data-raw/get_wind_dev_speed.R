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
