## HMS sharks from Large Pelagics Survey (LPS)
## Provided by HMS Cliff Hutt
## Last updated 2/6/2025
library(tidyverse)
library(readxl)

# Establish directory and define input file name
raw.dir <- here::here("data-raw")
lps_xlsx <- "LPS Pelagic Shark Harvest Estimates 2002-2024.xlsx"

get_lps_sharks <- function(save_clean = F){
  # Load and format data for SOE
  lps_sharks <- readxl::read_excel(file.path(raw.dir,lps_xlsx), sheet = "Sheet3") |>
    janitor::row_to_names(1) |>
    dplyr::select(-"(blank)")

  # Split dataset into MAB and NE subsets
  # MAB
  lps_sharks_mab <- lps_sharks[2:24,] |>
    dplyr::mutate(Units = "N of Fish",
                  EPU = "MAB") |>
    dplyr::rename("Total" = "Grand Total",
                  "Year" = "Row Labels",
                  "Blue_Shark" = "BLUE SHARK",
                  "Common_Thresher" = "COMMON THRESHER",
                  "Shortfin_Mako" = "SHORTFIN MAKO") |>
    tidyr::pivot_longer(cols = c("Blue_Shark",
                                 "Common_Thresher",
                                 "Shortfin_Mako",
                                 "Total"),
                        names_to = "Var", values_to = "Value") |>
    dplyr::select(Year, Var, Value, EPU, Units)

  # NE
  lps_sharks_ne <- lps_sharks[26:48,] |>
    dplyr::mutate(Units = "N of Fish",
                  EPU = "NE") |>
    dplyr::rename("Total" = "Grand Total",
                  "Year" = "Row Labels",
                  "Blue_Shark" = "BLUE SHARK",
                  "Common_Thresher" = "COMMON THRESHER",
                  "Shortfin_Mako" = "SHORTFIN MAKO") |>
    tidyr::pivot_longer(cols = c("Blue_Shark",
                                 "Common_Thresher",
                                 "Shortfin_Mako",
                                 "Total"),
                        names_to = "Var", values_to = "Value") |>
    dplyr::select(Year, Var, Value, EPU, Units)

  lps_sharks <- rbind(lps_sharks_mab, lps_sharks_ne)

  # metadata ----
  attr(lps_sharks, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/recreational-shark-fishing-indicators.html"
  attr(lps_sharks, "data_files")   <- list(
    lps_xlsx = lps_xlsx)
  attr(lps_sharks, "data_steward") <- c(
    "Kimberly Bastille <kimberly.bastille@noaa.gov>")

  if (save_clean){
    usethis::use_data(lps_sharks, overwrite = T)
  } else {
    return(lps_sharks)
  }
}
get_lps_sharks(save_clean = T)








