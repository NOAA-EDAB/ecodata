## HMS sharks occurence on the shelf
# Data from MRIP - https://www.fisheries.noaa.gov/recreational-fishing-data/recreational-fishing-data-downloads
# Seleted species from categories from Duartes lists from observer data
library(tidyverse)

raw.hms.dir <- here::here("data-raw/hms-mrip")
raw.dir <- here::here("data-raw")

get_lps_sharks <- function(save_clean = F){
  ## Bring in data
  lps_csv <- "LPS_shark_catch.csv"
  species_list <- "species_list.csv"
  hms_cat<- "hms_sp_category.csv"

  d <- read.csv(file.path(raw.dir,lps_csv))
  sp <- read.csv(file.path(raw.hms.dir,species_list)) %>%
    dplyr::rename(SP_CODE = sp_code) %>%
    dplyr::select(SP_CODE, COMMON_NAME)
  sp_cat <- read.csv(file.path(raw.hms.dir,hms_cat))

 region<- data.frame(State.Area = c("VIRGINIA" ,"MARYLAND/DELAWARE","SOUTH NEW JERSEY", "NORTH NEW JERSEY","NEW YORK",
            "CONNECTICUT/RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE/MAINE"),
                    EPU = c("MAB","MAB","MAB","MAB","MAB","MAB","NE", "NE"))

  ## select those in NEUS Shelf
  lps_sharks<- d %>%
    dplyr::rename("COMMON_NAME" = Species,
                  "Total_Catch" = "Total.Catch..Kept.Alive.Dead.") %>%
    dplyr::mutate(COMMON_NAME=recode(COMMON_NAME, 'PORBEAGLE SHARK'='PORBEAGLE',
                                     'COMMON THRESHER'='THRESHER SHARK')) %>%
    dplyr::filter(!Total_Catch == ".") %>%
    left_join(sp_cat, by= "COMMON_NAME") %>%
    left_join(region, by = "State.Area") %>%
    dplyr::group_by(Year, SP_CATEGORY, EPU) %>%
    dplyr::mutate(Total_Catch = as.numeric(Total_Catch)) %>%
    dplyr::summarise(Value = sum(Total_Catch)) %>%
    dplyr::rename( Time = Year,
                   Var = SP_CATEGORY) %>%
    dplyr::mutate(Units = "N of Fish")

  # metadata ----
  attr(lps_sharks, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/recreational-shark-fishing-indicators.html"
  attr(lps_sharks, "data_files")   <- list(
    lps_csv = lps_csv,
    hms_cat = hms_cat)
  attr(lps_sharks, "data_steward") <- c(
    "Kimberly Bastille <kimberly.bastille@noaa.gov>")

  if (save_clean){
    usethis::use_data(lps_sharks, overwrite = T)
  } else {
    return(lps_sharks)
  }
}
get_lps_sharks(save_clean = T)








