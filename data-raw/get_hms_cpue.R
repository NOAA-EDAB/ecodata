### HMS POP CPUE

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

raw.dir <- here::here("data-raw")
#HMS_SPUE_xlsx <- "HMS POP CPUE_Cudney - Jennifer Cudney - NOAA Federal.xlsx"
HMS_SPUE_csv <- "2024 Atlantic HMS POP CPUE Data - Jennifer Cudney - NOAA Federal.csv"
HMS_sp_csv <- "HMS_species_pdf.csv"

get_hms_cpue <- function(save_clean = F){
  code_csv<- read.csv(file.path(raw.dir,HMS_sp_csv))
  hms_cpue <- read.csv(file.path(raw.dir,HMS_SPUE_csv)) %>%
    dplyr::select(-X) %>%
    dplyr::rename(Code = Animal_Code,
                  Value = Count) %>%
    dplyr::mutate(Units =  "n",
                  EPU = "All") %>%
    left_join(code_csv) %>%
    dplyr::filter(!Common == "NA") %>%
    dplyr::rename(Var = Common,
                  Time = Year) %>%
    tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU)

  # metadata ----
  attr(hms_cpue, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc.html"
  attr(hms_cpue, "data_files")   <- list(
    HMS_SPUE_csv = HMS_SPUE_csv,
    HMS_sp_csv = HMS_sp_csv)
  attr(hms_cpue, "data_steward") <- c(
    "Jennifer Cudney <Jennifer.cudney@noaa.gov>")
  attr(hms_cpue, "plot_script") <- list(
    `mf_MAB_sharks` = "macrofauna_MAB.Rmd-hms-cpue-sharks.R")

  if (save_clean){
    usethis::use_data(hms_cpue, overwrite = T)
  } else {
    return(hms_cpue)
  }
}
get_hms_cpue(save_clean = T)






raw.dir <- here::here("data-raw")

HMS_sp_cat_csv <- "hms-mrip/hms_sp_category.csv"

get_hms_category <- function(save_clean = F){
  hms_category<- read.csv(file.path(raw.dir,HMS_sp_cat_csv)) %>%
    tibble::as_tibble()

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
