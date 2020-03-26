# Process raw common tern productivity and diet data

# If save == TRUE, processed data are saved as a .Rds file to the data directory.

# Function returns a processed data frame containing time series of productivity and diet
# composition for common tern at different islands in southern Gulf of Maine. The first
# three letters in the variable "Var" correspond to the island at which sampling occurred,
# with COTE referring to Common Tern. If "Var" includes productivity, then "Value" is the
# average number of fledged chicks.
# Otherwise, if "Var" specifies diet, then  "Value"  is equal to the count of observed prey type.
# For example, "EER COTE Diet Amphipod" refers to the number of preyed upon amphipods
# observed at Eastern Egg Rock in a given year.

library(dplyr)
library(tidyr)

#Get raw data
raw.dir <- here::here("data-raw")


get_seabird_ne <- function(save_clean = F){

  d <- read.csv(file.path(raw.dir,"Audubon_Common_Tern.csv"))

  #Process
  seabird_ne <- d %>%
    tidyr::pivot_longer(cols = Productivity:Mackerel, names_to = "Var", values_to = "Value") %>%
    tidyr::unite(., "Var", c("Island", "Species", "Var"), sep = " ") %>%
    dplyr::rename(Time = Year) %>%
    dplyr::mutate(EPU = "GOM",
                  Units = ifelse(stringr::str_detect(Var, "Productivity"),
                                 "fledged chicks per nest","N"))

  if (save_clean){

    usethis::use_data(seabird_ne, overwrite = T)
  } else {
    return(seabird_ne)
  }
}
get_seabird_ne(save_clean = T)


