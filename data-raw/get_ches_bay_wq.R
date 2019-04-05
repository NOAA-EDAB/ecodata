#Chesapeake bay water quality attainment indicator

library(dplyr)
library(tidyr)


raw.dir <- here::here("data-raw")

get_ches_bay_wq <- function(save_clean =F){
  ches_bay_wq <- read.csv(file.path(raw.dir, "Attainment_indicator.csv")) %>% 
    dplyr::select(Time = Year.1, Value = Total) %>% 
    mutate(Var = "chesapeake bay water quality attainment",
           Units = "estimated attainment, percent",
           EPU = "MAB")
  
  if (save_clean){
    usethis::use_data(ches_bay_wq, overwrite = T)
  } else {
    return(ches_bay_wq)
  }
}
