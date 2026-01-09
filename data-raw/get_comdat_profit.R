# Geret DePiper profitability indices
raw.dir <- here::here("data-raw")
comdat_profit_input <- "comdat_profit.rds"

get_comdat_profit <- function(save_clean){

  comdat_profit <- readRDS(file.path(raw.dir, comdat_profit_input))

  if (save_clean){
    usethis::use_data(comdat_profit, overwrite = T)
  } else {
    return(comdat_profit)
  }
}

get_comdat_profit(save_clean = T)
