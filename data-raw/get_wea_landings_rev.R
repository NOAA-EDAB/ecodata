## Landings and Revenue in wind lease areas

library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")

## Doug Christel
wind_xlsx<-"CHRISTEL_2022 State of the Ecosystem Report_Max WEA Species Landings and Revenue.xlsx"

get_wea_landings_rev <- function(save_clean = F){
  # import data
  wea_landings_rev<-read_excel(file.path(raw.dir,wind_xlsx)) %>%
    janitor::row_to_names(.,1) %>%
    dplyr::select( "GARFO and ASMFC Managed Species",
                   "Maximum Percent Total Annual Regional Species Landings",
                   "Maximum Percent Total Annual Regional Species Revenue",
                   "Minimum Percent Total Annual Regional Species Landings",
                   "Minimum Percent Total Annual Regional Species Revenue" ) %>%
    dplyr::rename("perc_landings_max" = "Maximum Percent Total Annual Regional Species Landings",
                  "perc_revenue_max" = "Maximum Percent Total Annual Regional Species Revenue" ,
                  "perc_landings_min" = "Minimum Percent Total Annual Regional Species Landings",
                  "perc_revenue_min" = "Minimum Percent Total Annual Regional Species Revenue") %>%
    dplyr::mutate(perc_landings_max = paste(as.numeric(perc_landings_max)*100, "%"),
                  perc_revenue_max = paste(as.numeric(perc_revenue_max)*100, "%"),
                  perc_landings_min = paste(as.numeric(perc_landings_min)*100, "%"),
                  perc_revenue_min = paste(as.numeric(perc_revenue_min)*100, "%"))

  if (save_clean){
    usethis::use_data(wea_landings_rev, overwrite = T)
  } else {
    return(wea_landings_rev)
  }
}
get_wea_landings_rev(save_clean = T)





