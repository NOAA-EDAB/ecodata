# Ingest and process Effective Swept Area data

# Define input file path
raw.dir <- here::here("data-raw")
effective_sweptarea_rdata <- "ATT87886.RData"

get_effective_sweptarea <- function(save_clean = F) {
  # Load original Effective Swept Area data file
  load(file.path(raw.dir, effective_sweptarea_rdata))

  effective_sweptarea <- final_sweptarea |>
    dplyr::rename(Time = YEAR, EPU = MGMT_AREA, Value = value) |>
    tidyr::unite(col = "Var", Measure, TRIP_TYPE, sep = "_") |>
    dplyr::select(Time, EPU, Var, Value)

  if (save_clean) {
    usethis::use_data(effective_sweptarea, overwrite = T)
  } else {
    return(effective_sweptarea)
  }
}
get_effective_sweptarea(save_clean = T)
