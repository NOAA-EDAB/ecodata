### HMS POP CPUE

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

raw.dir <- here::here("data-raw")
HMS_SPUE_xlsx <- "HMS POP CPUE_Cudney - Jennifer Cudney - NOAA Federal.xlsx"
HMS_sp_csv <- "HMS_species_pdf.csv"

get_hms_cpue <- function(save_clean = F){
  code_csv<- read.csv(file.path(raw.dir,HMS_sp_csv))
  hms_cpue <- read_excel(file.path(raw.dir,HMS_SPUE_xlsx)) %>%
    dplyr::select(-UID,-Count, -Haul_Num) %>%
    dplyr::rename(Code = Animal_Code,
                  Value = Num_per_haul) %>%
    dplyr::mutate(Units =  "n",
                  EPU = "All") %>%
    left_join(code_csv) %>%
    dplyr::filter(!Common == "NA") %>%
    dplyr::rename(Var = Common) %>%
    dplyr::select(Year, Value, EPU, Var)

  if (save_clean){
    usethis::use_data(hms_cpue, overwrite = T)
  } else {
    return(hms_cpue)
  }
  # metadata ----
  attr(hms_cpue, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc.html"
  attr(hms_cpue, "data_files")   <- list(
    HMS_SPUE_xlsx = HMS_SPUE_xlsx,
    HMS_sp_csv = HMS_sp_csv)
  attr(hms_cpue, "data_steward") <- c(
    "Jennifer Cudney <Jennifer.cudney@noaa.gov>")
}
get_hms_cpue(save_clean = T)






raw.dir <- here::here("data-raw")

HMS_sp_cat_csv <- "hms-mrip/hms_sp_category.csv"

get_hms_category <- function(save_clean = F){
  hms_category<- read.csv(file.path(raw.dir,HMS_sp_cat_csv))

  if (save_clean){
    usethis::use_data(hms_category, overwrite = T)
  } else {
    return(hms_category)
  }
  # metadata ----
  attr(hms_category, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc.html"
  attr(hms_category, "data_files")   <- list(
    HMS_sp_cat_csv = HMS_sp_cat_csv)
  attr(hms_category, "data_steward") <- c(
    "Jennifer Cudney <Jennifer.cudney@noaa.gov>")
}
get_hms_category(save_clean = T)
