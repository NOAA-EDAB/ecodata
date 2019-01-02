# Process stratification data

# These data are time series of average stratification (0-50 m depth) by EPU. 


library(dplyr)
library(tidyr)

get_stratification <- function(save_clean = F){
  
  raw.dir <- here::here("inst","extdata")
  
  strat <- read.csv(file.path(raw.dir, "Strat50.csv"), stringsAsFactors = FALSE)
  
  stratification <- strat %>% 
    dplyr::rename(Time = time, Var = var, Value = stratification) %>% 
    separate(., Var, c("Var","EPU"), sep = "_") %>% 
    mutate(Var = "stratification (0-50 m)",
           Units = "kg m^-3")
  
  if (save_clean){
    save(stratification, file = file.path(clean.dir, "stratification.Rds"))
  } else {
    return(stratification)
  }
  
}


