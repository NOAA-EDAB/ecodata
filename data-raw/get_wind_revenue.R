library(dplyr)
library(tidyr)
library(readxl)
raw.dir <- here::here("data-raw")
wind_rev_xlsx<- "CHRISTEL_2022 State of the Ecosystem Report_Max WEA Species Landings and Revenue (1).xlsx"

get_wind_revenue<- function(save_clean = F){
  ne_wind_revenue <- readxl::read_excel(file.path(raw.dir,wind_rev_xlsx), sheet = "NEFMC Revenue Figures") %>%
    dplyr::mutate(EPU = "NE")
  mab_wind_revenue <- readxl::read_excel(file.path(raw.dir,wind_rev_xlsx), sheet = "MAFMC Revenue Figure") %>%
    dplyr::mutate(EPU = "MAB")

  wind_revenue <- ne_wind_revenue %>%
    rbind(mab_wind_revenue) %>%
    tidyr::pivot_longer(cols = c("Sum of WEA_LANDED_TOTAL", "Sum of WEA_DOLLAR_TOTAL"),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::rename(Time = Year) %>%
    dplyr::mutate(Var = paste0(Species,":",Var)) %>%
    dplyr::select(!Species)
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
