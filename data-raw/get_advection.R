# Processing Advection index
raw.dir <- here::here("data-raw")
adv_input <- "NEUS_AdvIdx - Laurel Smith - NOAA Federal.csv"

get_advection <- function(save_clean = F) {
  advection <- read.csv(file.path(raw.dir, adv_input)) |>
    tidyr::unite("Time", Year, Month, sep = ".", remove = T) |>
    dplyr::rename("T15m" = Value, "T15mto40m" = Value.1, "TBlw40m" = Value.2) |>
    dplyr::select(Time, T15m, T15mto40m, TBlw40m, EPU, Units) |>
    tidyr::pivot_longer(
      c("T15m", "T15mto40m", "TBlw40m"),
      names_to = "Var",
      values_to = "Value"
    ) |>
    dplyr::mutate(
      EPU = dplyr::recode(EPU, "    GOMGB" = "NE", "      MAB" = "MAB")
    ) |>
    dplyr::mutate(
      Units = dplyr::recode(Units, "    10^6 m^3/s" = "10^6 m^3/s"),
      Time = as.numeric(Time)
    ) |>
    dplyr::select(Time, Var, Value, EPU, Units)

  if (save_clean) {
    usethis::use_data(advection, overwrite = T)
  } else {
    return(advection)
  }
}
get_advection(save_clean = T)
