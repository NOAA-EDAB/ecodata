# Processing for Gulf Stream Index data

# GSI = degrees latitude above the average Gulf Stream position based
# on ocean temperature at 200m (15 C) depth between 55W to 75W.
# Seasons are grouped by 12:2 = Winter, 3:5 = Spring, 6:8 = Summer, 9:11 = Fall

library(dplyr)
library(tidyr)
library(lubridate)

raw.dir <- here::here("data-raw")
gsi_xlsx<-"EN4_T200_GSI_1954_2020_monthly - Zhuomin Chen.xlsx"

get_ESP_gsi <- function(save_clean = F){

  ESP_gsi_season <- readxl::read_excel(file.path(raw.dir, gsi_xlsx)) %>%
    dplyr::rename(Time = Month, Value = GSI) %>%
    tidyr::separate(Time, into = c("year", "month")) %>%
    dplyr::mutate(month = as.character(month),
                  year = as.numeric(year),
      season_month = dplyr::recode(month,"01" = "Winter", "1" = "Winter", "02" = "Winter", "03" = "Spring",
                                 "04" = "Spring", "05" = "Spring", "06" = "Summer", "07" = "Summer",
                                 "08" = "Summer", "09" = "Fall", "10" = "Fall", "11" = "Fall", "12" = "Fall"),
      year = case_when(month == "12" ~ (year+1),
                       month!= "12" ~ year)) %>%
    group_by(year,season_month ) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    dplyr::mutate(Var = "gulf stream index - season",
                  Units = "latitude anomaly",
                  EPU = "All")


  ESP_gsi_month <- readxl::read_excel(file.path(raw.dir, gsi_xlsx)) %>%
    dplyr::rename(Time = Month, Value = GSI) %>%
    tidyr::separate(Time, into = c("year", "month")) %>%
    dplyr::mutate(month = as.character(month),
                  year = as.numeric(year),
                  season_month = dplyr::recode(month,"01" = "Jan", "1" = "Jan", "02" = "Feb", "03" = "Mar",
                                         "04" = "Apr", "05" = "May", "06" = "Jun", "07" = "Jul",
                                         "08" = "Aug", "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec"),
                  year = case_when(month == "12" ~ (year+1),
                                   month!= "12" ~ year)) %>%
    group_by(year,season_month ) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    dplyr::mutate(Var = "gulf stream index - month",
                  Units = "latitude anomaly",
                  EPU = "All")

  ESP_gsi <- ESP_gsi_season %>% rbind(ESP_gsi_month)

  # metadata ----
  attr(ESP_gsi, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/gulf-stream-index.html"
  attr(ESP_gsi, "data_files")   <- list(
    gsi_xlsx = gsi_xlsx)
  attr(ESP_gsi, "data_steward") <- c(
    "Vincent Saba <vincent.saba@noaa.gov>")
  attr(ESP_gsi, "plot_script") <- list(
    `ltl_MAB` = "LTL_MAB.Rmd-gsi.R")

  if (save_clean){
    usethis::use_data(ESP_gsi, overwrite = T)
  } else {
    return(ESP_gsi)
  }
}
get_ESP_gsi(save_clean = T)








# p<- ESP_gsi %>% filter(Var =="gulf stream index - season") %>%
#   ggplot2::ggplot(aes(x = year, y = Value))+
#   ggplot2::geom_point()+
#   ggplot2::geom_line()+
#   ecodata::geom_gls()+
#   ggplot2::facet_wrap(~season_month)
