# Geret DePiper profitability indices
raw.dir <- here::here("data-raw")
# comdat_profit_input <- "comdat_profit.rds"
comdat_profit_input <- "profitability_indices_by_area.rds"

get_comdat_profit <- function(save_clean) {
  comdat_profit <- readRDS(file.path(raw.dir, comdat_profit_input)) |>
    tidyr::pivot_longer(
      cols = -c(YEAR, EPU),
      names_to = "Var",
      values_to = "Value"
    ) |>
    dplyr::rename(Time = YEAR) |>
    dplyr::mutate(
      Var = dplyr::case_when(
        Var == "rindex" ~ "revenue_index",
        Var == "cindex" ~ "cost_index",
        Var == "prof_index" ~ "profit_index"
      )
    ) |>
    dplyr::select(Time, Var, Value, EPU)

  if (save_clean) {
    usethis::use_data(comdat_profit, overwrite = T)
  } else {
    return(comdat_profit)
  }
}

get_comdat_profit(save_clean = T)
