#### Get Community Social Vulnerability Indicators (CSVI)

library(dplyr)
library(tidyr)

# Set directory
raw.dir <- here::here("data-raw")

# Define input files
comm_social_vuln_csv <- "2023_National_Indicators_for__Northeast_120525.csv"
comm_social_old_csv = "fishdata_2022_CSVI_1-23-2025.csv"
ma_activity_csv = "MA_normalized_soe.csv"
ne_activity_csv = "NE_normalized_soe.csv"


get_engagement <- function(save_clean = F) {
  # Create data frame to define State-EPU
  EPUlook <- data.frame(
    State = c(
      "ME",
      "MA",
      "RI",
      "CT",
      "DE",
      "NH",
      "NY",
      "NJ",
      "MD",
      "VA",
      "NC",
      "PA"
    ),
    EPU = c(
      "NE",
      "NE",
      "NE",
      "NE",
      "MAB",
      "NE",
      "MAB",
      "MAB",
      "MAB",
      "MAB",
      "MAB",
      "MAB"
    )
  )

  # this reads numbers in as numbers
  #Get rec engagement
  dat.soc.old = readr::read_csv(file.path(raw.dir, comm_social_old_csv)) |>
    dplyr::rename("State" = "STATEABBR", 'totpotp' = 'TOTPOP') |>
    dplyr::left_join(EPUlook, by = "State") |>
    dplyr::select(year, EPU, GEO_NAME, dplyr::starts_with('Rec')) |>
    dplyr::mutate(dplyr::across(
      RecEng_ct:RecRel_ct,
      ~ as.numeric(dplyr::case_when(
        . == "low" ~ "1",
        . == "med" ~ "2",
        . == "med high" ~ "3",
        . == "high" ~ "4",
        TRUE ~ NA_character_
      ))
    ))

  dat.soc <- readr::read_csv(file.path(raw.dir, comm_social_vuln_csv)) |>
    dplyr::rename("State" = "STATEABBR") |>
    dplyr::left_join(EPUlook, by = "State")

  dat.act.ma = readr::read_csv(file.path(raw.dir, ma_activity_csv)) |>
    dplyr::mutate(EPU = "MAB") |>
    # Recode "OTHER VA" to conform with all other place_ids
    dplyr::mutate(
      place_id = dplyr::recode(
        place_id,
        "OTHER VA (includes REEDVILLE)" = "OTHER_VA"
      )
    )
  dat.act.ne = readr::read_csv(file.path(raw.dir, ne_activity_csv)) |>
    dplyr::mutate(EPU = 'NE')

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

  dat.soc.out <- dat.soc |>
    dplyr::bind_rows(dat.soc.old) |>
    # Filter only Northeast region
    #dplyr::filter(REGION == "Northeast") |>
    # Deselect unnecessary columns
    #dplyr::mutate(EPU = case_when(Council == "New England" ~ "NE",
    #                              Council == "Mid-Atlantic" ~ "MAB")) |>
    dplyr::select(
      !c("MAPNAME", "geography", "REGION", "State", "GEO_ID2", "SUMLEVEL")
    ) |>
    # can't do this and keep correct data attributes (numeric and factor)
    tidyr::pivot_longer(
      cols = c(
        "RecEng",
        "RecRel",
        "RecEng_ct",
        "RecRel_ct",
        "totpop",
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
        "urban_sprawl_index_rank"
      ),
      names_to = "Var",
      values_to = "Value"
    ) |>
    dplyr::mutate(
      Units = dplyr::case_when(
        Var == "totpop" ~ "number of individuals",
        Var != "totpop" ~ "unitless"
      )
    ) |>
    tidyr::unite("Var", c(GEO_NAME, Var), sep = "-") |>
    dplyr::rename("Time" = "year") |>
    dplyr::select(Time, Var, Value, EPU, Units) |>
    dplyr::filter(!is.na(Time))

  dat.act.out = dat.act.ma |>
    dplyr::bind_rows(dat.act.ne) |>
    dplyr::mutate(place_id2 = gsub('_', ', ', place_id)) |>
    dplyr::rename(Time = 'YEAR') |>
    tidyr::pivot_longer(
      cols = c('fishing_mean_score'),
      names_to = 'Var',
      values_to = "Value"
    ) |>
    tidyr::unite('Var', c(place_id2, Var), sep = '-') |>
    dplyr::mutate(Units = 'unitless') |>
    dplyr::select(Time, Var, Value, EPU, Units)

  engagement = dat.soc.out |>
    dplyr::bind_rows(dat.act.out)

  if (save_clean) {
    usethis::use_data(engagement, overwrite = T)
  } else {
    return(engagement)
  }
}
get_engagement(save_clean = T)
