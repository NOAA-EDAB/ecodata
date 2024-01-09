## get condition
## condition factor calculations from Laurel Smith using
## BTS data by species aggregated to annual condition index.

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
condition_csv <- "RelCond2023_Year.csv"

get_condition <- function(save_clean = F){
  dat <- read.csv(file.path(raw.dir,condition_csv))

  condition <- dat %>%
    tidyr::pivot_longer(cols = c(MeanCond),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::select(YEAR, Species, EPU, Value) %>%
    dplyr::rename(Time = YEAR,
                  Var = Species) %>%
    dplyr::mutate(Units = c("MeanCond"))

  # metadata ----
  attr(condition, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/condition.html"
  attr(condition, "data_files")   <- list(
    condition_csv = condition_csv)
  attr(condition, "data_steward") <- c(
    "Laurel Smith <laurel.smith@noaa.gov>")


  if (save_clean){
    usethis::use_data(condition, overwrite = T)
  } else {
    return(condition)
  }
}
get_condition(save_clean = T)
