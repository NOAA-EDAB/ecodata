library(dplyr)
library(tidyr)
library(readxl)
raw.dir <- here::here("data-raw")
wind_rev_xlsx<- "WEA_landings_revenue_2008-2023_SOE 2025 - Douglas Christel - NOAA Federal.xlsx"

get_wind_revenue<- function(save_clean = F){
  ne_wind_revenue <- readxl::read_excel(file.path(raw.dir,wind_rev_xlsx),
                                        sheet = "NEFMC Land-Revenue Figures") %>%
    janitor::row_to_names(.,1) %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(EPU = "NE")

  ne_wind_revenue <- ne_wind_revenue[-c(294:407),] %>%
    dplyr::mutate(Species = dplyr::recode(Species, "All Managed Skates" = "Skates"))

  mab_wind_revenue <- readxl::read_excel(file.path(raw.dir,wind_rev_xlsx),
                                         sheet = "MAFMC Land-Revenue Figures") %>%
    janitor::row_to_names(.,1) %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(EPU = "MAB")

  mab_wind_revenue <- mab_wind_revenue[-241,] %>%
    dplyr::mutate(Species = dplyr::recode(Species, "SCUP" = "Scup"))

    #for (i in 2012:2016){
    #  mab_wind_revenue <- mab_wind_revenue %>%
    #  rbind(c("Ocean Quahog", i, NA, NA, NA, NA, NA, NA, NA, NA, "MAB"))
    #}

    #for (i in 2018:2022){
    #  mab_wind_revenue <- mab_wind_revenue %>%
    #  rbind(c("Ocean Quahog", i, NA, NA, NA, NA, NA, NA, NA, NA, "MAB"))
    #}

  mab_wind_revenue <- mab_wind_revenue %>%
    dplyr::group_by(Species) %>%
    dplyr::arrange(Year, .by_group = T)

  wind_revenue <- ne_wind_revenue %>%
    rbind(mab_wind_revenue) %>%
       dplyr::rename(Time = Year,
                 sum_value = "Revenue",
                 sum_landing = "Landings") %>%
    tidyr::pivot_longer(cols = c(sum_landing, sum_value),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Var = paste0(Species, "-", Var)) %>%
    dplyr::select(!Species) %>%
    tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU) %>%
    dplyr::mutate(Value = as.double(Value),
                  Time = as.integer(Time)) %>%
    dplyr::mutate(Value = dplyr::na_if(Value, 0))

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
