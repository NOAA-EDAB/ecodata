### Cold pool index from Chris Melrose

library(dplyr)
library(tidyr)


raw.dir <- here::here("data-raw")

get_cold_pool <- function(save_clean = F){

  cold_pool <- read.csv(file.path(raw.dir, "cold_pool_index.csv")) %>%
    dplyr::rename(EPU = Region,
                  Time = Year,
                  Value = VAR)

  if(save_clean){
    usethis::use_data(cold_pool, overwrite = T)
  } else {
    return(cold_pool)
  }


}
get_cold_pool(save_clean = T)
