#### Get Community Social Vulnerability Indicators (CSVI)

library(dplyr)
library(tidyr)

# Set directory
raw.dir <- here::here("data-raw")

# Define input files
comm_social_vuln_csv <- "fishdata_2022_CSVI_1-23-2025.csv"

get_engagement <- function(save_clean = F){
  # Create data frame to define State-EPU
  EPUlook <- data.frame(State = c("ME", "MA", "RI", "CT", "DE", "NH", "NY", "NJ", "MD", "VA", "NC", "PA"),
                  EPU = c("NE", "NE", "NE", "NE", "MAB", "NE","MAB","MAB","MAB","MAB","MAB","MAB"))

  # this reads numbers in as numbers
  dat <- readr::read_csv(file.path(raw.dir,comm_social_vuln_csv))

  # These are Bobby's data edits
  #rename top communities
  dat$GEO_NAME <- replace(dat$GEO_NAME,
                          dat$GEO_NAME=="Port Clyde-Tenants Harbor/Saint George/Spruce Head, ME",
                          "Port Clyde-Tenants Harbor, ME")


  dat$GEO_NAME <- replace(dat$GEO_NAME,
                          dat$GEO_NAME=="Sandwich/East Sandwich/Forestdale, MA",
                          "Sandwich, MA")


  dat$GEO_NAME <- replace(dat$GEO_NAME,
                          dat$GEO_NAME=="Reedville/District 5 (Northumberland County), VA",
                          "Reedville, VA")

  # If we want a long dataset, these need to be numbers to have them with the Eng and Rel values
  # Otherwise we need a wide dataset
  dat<- dat |>
    dplyr::mutate(across(ComEng_ct:urban_sprawl_index_rank, ~
                           as.numeric(case_when(
                             . == "low" ~ "1",
                             . == "med" ~ "2",
                             . == "med high" ~ "3",
                             . == "high" ~ "4",
                             TRUE ~ NA_character_
                           ))))



  engagement <- dat |>
    # Filter only Northeast region
    #dplyr::filter(REGION == "Northeast") |>
    # Deselect unnecessary columns
    dplyr::mutate(EPU = case_when(Council == "New England" ~ "NE",
                                  Council == "Mid-Atlantic" ~ "MAB")) |>
    dplyr::select(!c("Council", "MAPNAME", "geography", "REGION", "STATEABBR")) |>
    # can't do this and keep correct data attributes (numeric and factor)
    tidyr::pivot_longer(cols = c("TOTPOP",
                                 "personal_disruption",
                                 "pop_composition",
                                 "poverty",
                                 "labor_force_str",
                                 "housing_characteristics",
                                 "housing_disrupt",
                                 "retiree_migration",
                                 "urban_sprawl_index",
                                 "ComEng",
                                 "ComRel",
                                 "RecEng",
                                 "RecRel",
                                 "RecEng_ct",
                                 "RecRel_ct",
                                 "ComEng_ct",
                                 "ComRel_ct",
                                 "personal_disruption_rank",
                                 "pop_composition_rank",
                                 "poverty_rank",
                                 "labor_force_str_rank",
                                 "housing_characteristics_rank",
                                 "housing_disrupt_rank",
                                 "retiree_migration_rank",
                                 "urban_sprawl_index_rank"),
                          names_to = "Var", values_to = "Value") |>
    dplyr::mutate(Units = case_when(Var == "TOTPOP" ~ "number of individuals",
                                    Var != "TOTPOP" ~ "unitless")) |>
    tidyr::unite("Var", c(GEO_NAME,Var), sep = "-") |>
    dplyr::rename("Time" = "year") |>
    dplyr::select(Time, Var, Value, EPU, Units)

  if (save_clean){
    usethis::use_data(engagement, overwrite = T)
  } else {
    return(engagement)
  }
}
get_engagement(save_clean = T)
