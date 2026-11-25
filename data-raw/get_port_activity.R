# Creates port_activity data object
port.activity.file <- here::here(
  'data-raw',
  'PCFA_share2010_11-17-2026 - Robert Murphy - NOAA Federal.csv'
)

get_port_activity <- function(save_clean = F) {
  port_activity_raw <- read.csv(port.activity.file)

  port_activity <- port_activity_raw |>
    dplyr::rename(Time = 'year', State = 'STATE_ABB', City = 'PORT_NAME') |>
    dplyr::mutate(
      EPU = dplyr::case_when(
        State %in% c('ME', 'NH', 'VT', 'CT', 'RI', 'MA') ~ "NE",
        State %in% c('NJ', 'NY', 'VA', 'PA', 'MD', 'NC', 'DE', 'WV') ~ "MAB",
        TRUE ~ NA
      ),
      City = dplyr::case_when(
        place_id == 'REEDVILLE_VA' ~ "Other VA (includes Reedville)",
        TRUE ~ City
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with('port'),
      names_to = 'Var',
      values_to = "Value"
    ) |>
    dplyr::mutate(City = stringr::str_to_title(City), Units = NA) |>
    dplyr::select(Time, EPU, City, State, Var, Value, Units)

  if (save_clean) {
    usethis::use_data(port_activity, overwrite = T)
  } else {
    return(port_activity)
  }
}

get_port_activity(save_clean = T)
