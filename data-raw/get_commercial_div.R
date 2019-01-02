# Process commercial diversity data

# More information about these data are available at https://noaa-edab.github.io/tech-memo/catch-and-fleet-diversity.html.
# Data are drawn from blend of VTR trip data, CFDBS prices, vessel characteristics from PERMIT databases, 
# and major VTR gear by permit. Here "MA" and "NE" refer to the Mid-Atlantic and New England regions respectively, and are 
# not derived by EPU.

library(dplyr)
library(tidyr)

raw.dir <- here::here("inst","extdata")
clean.dir <- here::here("data")

get_commercial_div <- function(save_clean = F){
  comm_div <- read.csv(file.path(raw.dir, "Commercial_Diversity_2018.csv")) %>%
    dplyr::select(-X, -Source) %>%
    dplyr::rename(EPU = Region) %>% 
    as.data.frame()
  
  if(save_clean){
    save(comm_div, file = file.path(clean.dir, "commercial_diversity.Rds"))
  } else {
    return(comm_div)
  }
  
  
}