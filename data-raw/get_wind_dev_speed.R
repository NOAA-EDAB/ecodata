## Wind develop areas

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

raw.dir <- here::here("data-raw")
wind_dev_speed_xlsx_2021<- "Tables_Cumulative Totals by Construction Year_01_20_21.xlsx"
wind_dev_speed_xlsx_2022<- "Wind Cumulative Data_SoE2021_2022 differences_12121.xlsx"
wind_dev_speed_xlsx_2023<- "wind_footprint_2023 - Angela Silva - NOAA Affiliate.xlsx"

get_wind_dev_speed <- function(save_clean = F){
  wind_dev_speed_2021 <- read_excel(file.path(raw.dir,wind_dev_speed_xlsx_2021), sheet = 3) %>%
    janitor::row_to_names(row_number = 1) %>%
    dplyr::rename(Time = "Year End",
                  Value = "Project Acres") %>%
    dplyr::mutate( Var = c("Tot_Area_Acres"),
                  EPU = c("ALL"))%>%
    dplyr::select( Time, Value, Var, EPU) %>%
    dplyr::filter(!Value == "NA") %>%
    dplyr::mutate(Report_year = c("year2021"))

  wind_dev_speed_2022 <- read_excel(file.path(raw.dir,wind_dev_speed_xlsx_2022), sheet = 2) %>%
    dplyr::rename(Time = "Turbines in the water (construction year)",
                  Tot_Area_Acres = "Project Area (acres)",
                  Num_Foundations = "No. of Foundations",
                  Cable_Acres = "Offshore Export Cable (acres)",
                  Cable_Miles = "Offshore + Inter-array Cable (Miles)",
                  Num_Projects = "# of projects constructed") %>%
    dplyr::filter(!Time == "Construction is estimated to be 2 years for each project.") %>%
    tidyr::pivot_longer(cols = c(  Tot_Area_Acres,Num_Foundations, Cable_Acres,
                                   Cable_Miles, Num_Projects),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(#Var = paste0(Var, "::", Location),
      Time = as.numeric(Time),
      Value = as.numeric(gsub(",","",Value)),
      EPU = c("All")) %>%
    dplyr::filter(!Value == "NA") %>%
    dplyr::mutate(Report_year = c("year2022"))

  wind_dev_speed_2023 <- read_excel(file.path(raw.dir,wind_dev_speed_xlsx_2023)) %>%
    tidyr::pivot_longer(cols = c("Acres", "Gen_Cap", "Fndns", "Exp_Cab",
                                 "Inter_Cab", "Cab_Total"),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::filter(!Const_Yr == "Beyond 2030 (Planning Areas)") %>%
    dplyr::mutate(Time = as.numeric(Const_Yr),
                  EPU = c("All"),
                  Report_year = c("year2023")) %>%
    dplyr::select(!Const_Yr)


    wind_dev_speed<- rbind(wind_dev_speed_2021, wind_dev_speed_2022,
                           wind_dev_speed_2023)

  if (save_clean){
    usethis::use_data(wind_dev_speed, overwrite = T)
  } else {
    return(wind_dev_speed)
  }
}
get_wind_dev_speed(save_clean = T)




