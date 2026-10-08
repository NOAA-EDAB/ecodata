# Ingest and process Crew Survey data

# Define input path
raw.dir <- here::here("data-raw")
crew_survey_xlsx <- "2012_2024_Crew Survey SOE Dataset_01052026.xlsx"

get_crew_survey <- function(save_clean = F) {
  # Read in original Crew Survey data file
  crew_survey_raw <- readxl::read_xlsx(
    path = file.path(raw.dir, crew_survey_xlsx)
  )

  crew_survey <- crew_survey_raw |>
    dplyr::mutate(
      ResponseID = c(1:nrow(crew_survey_raw)),
      Time = sub("-.*", "", `Survey wave`)
    ) |>
    dplyr::select(
      -`Subject number`,
      -`Case ID...2`,
      -`Case ID...3`,
      -`Survey wave`
    ) |>
    tidyr::pivot_longer(
      cols = -c(ResponseID, Time),
      names_to = "Var",
      values_to = "Value",
      values_transform = list(Value = as.character)
    )

  if (save_clean) {
    usethis::use_data(crew_survey, overwrite = T)
  } else {
    return(crew_survey)
  }
}
get_crew_survey(save_clean = T)
