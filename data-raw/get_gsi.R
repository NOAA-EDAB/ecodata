# Processing for Gulf Stream Index data

# GSI = degrees latitude above the average Gulf Stream position based
# on ocean temperature at 200m (15 C) depth between 55W to 75W.

library(dplyr)
library(tidyr)
library(lubridate)

raw.dir <- here::here("inst","extdata")

get_gsi <- function(save_clean = F){

  gsi <- read.csv(file.path(raw.dir, "GSI.csv")) %>% 
    dplyr::rename(Time = Year, Value = GSI) %>% 
    mutate(Var = "gulf stream index",
           Units = "latitude anomaly",
           EPU = "All")
  
  if (save_clean){
    usethis::use_data(gsi, overwrite = T)
  } else {
    return(gsi)
  }
}
