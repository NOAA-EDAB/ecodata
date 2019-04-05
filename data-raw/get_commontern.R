# Process raw common tern productivity and diet data

# If save == TRUE, processed data are saved as a .Rds file to the data directory. 

# Function returns a processed data frame containing time series of productivity and diet
# composition for common tern at different islands in southern Gulf of Maine. The first three letters
# in the variable "Var" correspond to the island at which sampling occurred, with COTE referring to 
# Common Tern. If "Var" includes productivity, then "Value" is the average number of fledged chicks.
# Otherwise, if "Var" specifies diet, then  "Value"  is equal to the count of observed prey type. 
# For example, "EER COTE Diet Amphipod" refers to the number of preyed upon amphipods observed at Eastern
# Egg Rock in a given year. 

library(dplyr)
library(tidyr)

#Get raw data
raw.dir <- here::here("data-raw")


get_commontern <- function(save_clean = F){

  d <- read.csv(file.path(raw.dir,"Audubon SRP Common Tern Data.csv"))
  
  #Process
  common_tern <- d %>%
    filter(Island != "") %>% 
    tidyr::gather(., Var, Value, -Year, -Species, -Island) %>%
    mutate(Species = ifelse(Var != "Productivity",
                                   paste(Species, "Diet"), "COTE")) %>% 
    unite(., "Var", c("Island", "Species", "Var"), sep = " ") %>% 
    dplyr::rename(Time = Year) %>% 
    mutate(EPU = "GOM",
                  Units = ifelse(stringr::str_detect(Var, "Productivity"),
                                 "fledged chicks per nest","N")) 
  
  if (save_clean){

    usethis::use_data(common_tern, overwrite = T)
  } else {
    return(common_tern)
  }
}

  

