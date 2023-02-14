library(dplyr)
library(tidyr)
library(readxl)
raw.dir <- here::here("data-raw")
wind_rev_xlsx<- "SOE 2023 update_Offshore Wind Fishery Data_Christel (3).xlsx"

get_wind_revenue<- function(save_clean = F){
  ne_wind_revenue <- readxl::read_excel(file.path(raw.dir,wind_rev_xlsx),
                                        sheet = "NEFMC Rev Fig Leases+Cent Atl") %>%
    #janitor::row_to_names(.,1) %>%
    #dplyr::select("Species","Year",
    #              "Sum of Nominal Value ($)",
    #              "Sum of Landings (pounds*)") %>%
    dplyr::mutate(EPU = "NE")
  mab_wind_revenue <- readxl::read_excel(file.path(raw.dir,wind_rev_xlsx),
                                         sheet = "MAFMC Rev Fig Leases+Cent Atl") %>%
    #janitor::row_to_names(.,1) %>%
    dplyr::mutate(EPU = "MAB") #%>%
    #dplyr::select("Species","Year", "EPU",
    #              "Sum of Nominal Value ($)",
    #              "Sum of Landings (pounds*)")

  wind_revenue <- ne_wind_revenue %>%
    rbind(mab_wind_revenue) %>%
    dplyr::rename(Time = Year,
                 sum_value = "Sum of total_wea_value",
                 sum_landing = "Sum of total_wea_landed") %>%
    tidyr::pivot_longer(cols = c(sum_landing, sum_value),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Var = paste0(Species, "-", Var)) %>%
    dplyr::select(!Species) %>%
    tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU)
  # metadata ----
  attr(wind_revenue, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc.html"
  attr(wind_revenue, "data_files")   <- list(
    wind_rev_xlsx = wind_rev_xlsx)
  attr(wind_revenue, "data_steward") <- c(
    "Geret DePiper <geret.depiper@noaa.gov>")
  attr(wind_revenue, "plot_script") <- list(
    `hd_MAB` = "human_dimensions_MAB.Rmd-wind-revenue.R",
    `hd_NE` = "human_dimensions_NE.Rmd-wind-revenue.R")

  if (save_clean){
    usethis::use_data(wind_revenue, overwrite = TRUE)
  } else {
    return(wind_revenue)
  }
}
get_wind_revenue(save_clean = T)
