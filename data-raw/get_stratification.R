# Process stratification data

# These data are time series of average stratification (0-50 m depth) by EPU. 


library(dplyr)
library(tidyr)

raw.dir <- here::here("inst","extdata")

get_stratification <- function(save_clean = F){
  
  strat <- read.csv(file.path(raw.dir, "Strat50.csv"), stringsAsFactors = FALSE)
  
  stratification <- strat %>% 
    dplyr::rename(Time = time, Var = var, Value = stratification) %>% 
    separate(., Var, c("Var","EPU"), sep = "_") %>% 
    mutate(Var = "stratification (0-50 m)",
           Units = "kg m^-3")
  
  if (save_clean){
    usethis::use_data(stratification, overwrite = T)
  } else {
    return(stratification)
  }
  
}


