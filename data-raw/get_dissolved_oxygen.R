# Processing for spatial dissolved oxygen
raw.dir <- here::here("data-raw")
dissolved_oxygen_input <- "merged_fishbot_glider_2025_daily_DO.csv"

get_dissolved_oxygen <- function(save_clean = F) {
  dissolved_oxygen <- read.csv(file.path(raw.dir, dissolved_oxygen_input)) |>
    dplyr::select(!c(grid_id, data_providers_adjusted, stat_area, depth)) |>
    dplyr::rename(Time = time, Lon = centroid_lon, Lat = centroid_lat) |>
    tidyr::pivot_longer(
      c(
        dissolved_oxygen,
        dissolved_oxygen_count,
        dissolved_oxygen_min,
        dissolved_oxygen_max
      ),
      names_to = "Var",
      values_to = "Value"
    ) |>
    dplyr::mutate(Time = as.Date(Time), EPU = 'All')

  if (save_clean) {
    usethis::use_data(dissolved_oxygen, overwrite = T)
  } else {
    return(dissolved_oxygen)
  }
}
get_dissolved_oxygen(save_clean = T)
