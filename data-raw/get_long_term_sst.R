#process ERSST long-term SST data

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")

get_long_term_sst <- function(save_clean = F){
  long_term_sst <- read.csv(file.path(raw.dir,"ersst annual mean.csv")) %>%
    dplyr::rename(Time = Year,
                  Value = Mean) %>%
    mutate(Var = "long-term sst",
           EPU = "All",
           Units = "degreesC")

  if (save_clean) {
    usethis::use_data(long_term_sst, overwrite = T)
  } else {
    return(long_term_sst)
  }

}
