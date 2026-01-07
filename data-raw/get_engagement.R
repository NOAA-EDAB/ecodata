#### Get Community Social Vulnerability Indicators (CSVI)

library(dplyr)
library(tidyr)

# Set directory
raw.dir <- here::here("data-raw")

# Define input files
comm_social_vuln_csv <- "2023_National_Indicators_for__Northeast_120525.csv"

get_engagement <- function(save_clean = F){
  # Create data frame to define State-EPU
  EPUlook <- data.frame(State = c("ME", "MA", "RI", "CT", "DE", "NH", "NY", "NJ", "MD", "VA", "NC", "PA"),
                  EPU = c("NE", "NE", "NE", "NE", "MAB", "NE","MAB","MAB","MAB","MAB","MAB","MAB"))

  # this reads numbers in as numbers
  dat <- readr::read_csv(file.path(raw.dir,comm_social_vuln_csv)) |>
    dplyr::rename("State" = "STATEABBR") |>
    dplyr::left_join(EPUlook, by = "State")

  # If we want a long dataset, these need to be numbers to have them with the Eng and Rel values
  # Otherwise we need a wide dataset
  #dat<- dat |>
    #dplyr::mutate(across(ComEng_ct:urban_sprawl_index_rank, ~
                           #as.numeric(case_when(
                           #   . == "low" ~ "1",
                           #  . == "med" ~ "2",
                           #   . == "med high" ~ "3",
                           #  . == "high" ~ "4",
                           #   TRUE ~ NA_character_
                           #))))

  engagement <- dat |>
    # Filter only Northeast region
    #dplyr::filter(REGION == "Northeast") |>
    # Deselect unnecessary columns
    #dplyr::mutate(EPU = case_when(Council == "New England" ~ "NE",
    #                              Council == "Mid-Atlantic" ~ "MAB")) |>
    dplyr::select(!c("MAPNAME", "geography", "REGION", "State", "GEO_ID2", "SUMLEVEL")) |>
    # can't do this and keep correct data attributes (numeric and factor)
    tidyr::pivot_longer(cols = c("totpop",
                                 "personal_disruption",
                                 "pop_composition",
                                 "poverty",
                                 "labor_force_str",
                                 "housing_characteristics",
                                 "housing_disrupt",
                                 "retiree_migration",
                                 "urban_sprawl_index",
                                 "personal_disruption_rank",
                                 "pop_composition_rank",
                                 "poverty_rank",
                                 "labor_force_str_rank",
                                 "housing_characteristics_rank",
                                 "housing_disrupt_rank",
                                 "retiree_migration_rank",
                                 "urban_sprawl_index_rank"),
                          names_to = "Var", values_to = "Value") |>
    dplyr::mutate(Units = case_when(Var == "totpop" ~ "number of individuals",
                                    Var != "totpop" ~ "unitless")) |>
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
