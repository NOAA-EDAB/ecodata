# Processing for preyfield energy density
raw.dir <- here::here("data-raw")
preyfield_energy_input <- "ZooData_RW_100m3_v9 - Gregory Ellis - NOAA Affiliate.csv"

get_preyfield_energy <- function(save_clean = F){

  preyfield_energy <- read.csv(file.path(raw.dir, preyfield_energy_input)) |>
    dplyr::select(c(LATITUDE, LONGITUDE, REGION, DAY, MONTH, YEAR, VOLUME_100M3,
                    TOTAL_CONC)) |>
    tidyr::unite(Time, YEAR, MONTH, DAY, sep = "-") |>
    dplyr::mutate(Time = as.Date(Time)) |>
    dplyr::rename(Lon = LONGITUDE, Lat = LATITUDE, EPU = REGION) |>
    tidyr::pivot_longer(c(VOLUME_100M3, TOTAL_CONC),
                        names_to = "Var", values_to = "Value") |>
    dplyr::select(Time, Lat, Lon, Var, Value, EPU)

  if (save_clean){
    usethis::use_data(preyfield_energy, overwrite = T)
  } else {
    return(preyfield_energy)
  }
}
get_preyfield_energy(save_clean = T)
