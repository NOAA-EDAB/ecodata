# Get shoreside support indicator
raw.dir <- here::here("data-raw/")

# Define input file path
shore_supp_csv <- "Shoreside_Support_2025 - Geret DePiper - NOAA Federal.csv"

get_shoreside_support <- function(save_clean = F){

  # Load data file
  shoreside_support <- read.csv(file.path(raw.dir, shore_supp_csv)) |>
    dplyr::select(!X) |>
    dplyr::rename(EPU = Region) |>
    dplyr::mutate(EPU = dplyr::recode(EPU, "MA" = "MAB"))

  if (save_clean){
    usethis::use_data(shoreside_support, overwrite = T)
  } else {
    return(shoreside_support)
  }
}
get_shoreside_support(save_clean = T)
