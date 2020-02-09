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

get_ne_inshore_survey <- function(save_clean = F){

  ne_inshore_survey <- read_excel(file.path(raw.dir, "MENH_TrawlSurvey.xlsx")) %>%
    dplyr::rename(Var = SOE.20,
                  Value = StratMean_Weight,
                  Time = Year) %>%
    dplyr::mutate(EPU = "NE",
                  Units = "(KG/tow)") %>%
    dplyr::select(-...1,-CV_Weight, - SE_Weight,
                  -Low_CI_Weight, -High_CI_Weight, -Survey ) %>%
    mutate(Season = recode(Season, "FL"= "Fall","SP" = "Spring" )) %>%
    unite(.,Var, c("Var","Season"),sep = " ")
  if (save_clean){
    usethis::use_data(ne_inshore_survey, overwrite = T)
  } else {
    return(list(ne_inshore_survey))
  }

}

get_ne_inshore_survey_species <- function(save_clean = F){
  ne_inshore_survey_species <- read_excel(file.path(raw.dir, "MENH_TrawlSpeciesinSOE2020.xlsx")) %>%
    mutate(Var = "Maine and NH inshore survey species") %>%
    rename(Species = CommonName,
           Group = SOE.20) %>%
    dplyr::select(-ITISSPP, -COMNAME, -SVSPP, -SCINAME)

  if (save_clean){
    usethis::use_data(ne_inshore_survey_species, overwrite = T)
  } else {
    return(list( ne_inshore_survey_species))
  }

}
get_ne_inshore_survey(save_clean = T)
get_ne_inshore_survey_species(save_clean = T)
