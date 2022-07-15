# Code for processing Maine and New Hampshire state survey data.

# Data definitions--------------------------------------------------------------------------------------

# Survey: This variable is used by us to identify the season and year of the survey FL
# is for Fall and SP is for spring. FL00 is Fall 2000, SP01 is Spring 2001, and so on.

# Var: Includes SOE species grouping, season that survey occurred (fall or spring), and variable
# calculated. The specific variables included are stratified mean biomass with associated confidence
# intervals, coefficients of variation, and standard errors.

# Time: Year that survey occurred.

# Methodology-------------------------------------------------------------------------------------------
# Metrics for biomass were calculated by:

# 1.	All species catch weights were summed up for each tow for each group.
# 2.	Then the average weight per tow and associated variances and standard deviation for each
# survey, region, stratum, and species group was calculated.
# 3.	The average weight for each group was then weighted by total area surveyed in each region
# and stratum. (average weight * area surveyed).
# 4.	The new weighted averages were then summed up by survey and species group then the totals
# were divided by the total area of the survey (11699.831 km2) to provide the stratified mean biomass
# for each species group in each survey. The coefficient of variation, standard error, and 95%
# confidence intervals were also calculated for each species group and provided.




library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

raw.dir <- here::here("data-raw")
ne_inshore_survey_rda <- "MENH_TrawlSurvey_SOE_Data.RData"
ne_inshore_survey_species_xlsx <- "MENH_TrawlSpeciesinSOE2020.xlsx"

get_ne_inshore_survey <- function(save_clean = F){

  load(file.path(raw.dir, ne_inshore_survey_rda))
  ne_inshore_survey <- Trawl.Strat4Indices %>%
    dplyr::rename(Var = SOE.20,
                  Value = StratMean_Weight,
                  Time = Year) %>%
    dplyr::mutate(EPU = "NE",
                  Units = "(KG/tow)") %>%
    dplyr::select(-CV_Weight, - SE_Weight,
                  -Low_CI_Weight, -High_CI_Weight, -Survey ) %>%
    dplyr::mutate(Season = dplyr::recode(Season, "FL"= "Fall","SP" = "Spring" )) %>%
    tidyr::unite(.,Var, c("Var","Season"),sep = " ")%>%
    dplyr::select(Time, Var, Value, EPU, Units)
  if (save_clean){
    usethis::use_data(ne_inshore_survey, overwrite = T)
  } else {
    return(list(ne_inshore_survey))
  }
  # metadata ----
  attr(ne_inshore_survey, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/inshoresurvdat.html"
  attr(ne_inshore_survey, "data_files")   <- list(
    ne_inshore_survey_csv = ne_inshore_survey_rda)
  attr(ne_inshore_survey, "data_steward") <- c(
    "Rebecca Peters <rebecca.j.peters@maine.gov>")
}






get_ne_inshore_survey_species <- function(save_clean = F){
  ne_inshore_survey_species <- read_excel(file.path(raw.dir, ne_inshore_survey_species_xlsx)) %>%
    dplyr::mutate(Var = "Maine and NH inshore survey species") %>%
    dplyr::rename(Species = CommonName,
           Group = SOE.20) %>%
    dplyr::select(-ITISSPP, -COMNAME, -SVSPP, -SCINAME)

  # metadata ----
  attr(ne_inshore_survey_species, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/inshoresurvdat.html"
  attr(ne_inshore_survey_species, "data_files")   <- list(
    ne_inshore_survey_species_xlsx = ne_inshore_survey_species_xlsx)
  attr(ne_inshore_survey_species, "data_steward") <- c(
    "Rebecca Peters <rebecca.j.peters@maine.gov>")
  attr(ne_inshore_survey, "plot_script") <- list(
    `mf_NE` = "macrofauna_NE.Rmd-ne-inshore-survey.R")

  if (save_clean){
    usethis::use_data(ne_inshore_survey_species, overwrite = T)
  } else {
    return(list( ne_inshore_survey_species))
  }
}
get_ne_inshore_survey(save_clean = T)
get_ne_inshore_survey_species(save_clean = T)
