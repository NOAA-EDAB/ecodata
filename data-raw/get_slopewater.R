# Process slopewater proportion time series
#
# Slopewater proportions give the percent total of water type observed in
# the deep Northeast Channel (150-200 m depth).
#
# Raw data fields correspond to year, water mass flavor (WSW = Warm Slope Water, LSLW = Labrador Slope Water),
# and proportion of total expressed as a percentage.


library(dplyr)
library(tidyr)

#Get raw
raw.dir <- here::here("data-raw") #input raw

get_slopewater <- function(save_clean = F){

  d <- read.csv(file.path(raw.dir,"slopewater_proportions.csv"))

  slopewater <- d %>%
    dplyr::rename(Time = year, Var = water.mass.flavor, Value = prop) %>%
    mutate(EPU = "GOM", Units = "unitless", Var2 = "proportion ne channel") %>%
    unite(.,Var,c(Var,Var2), sep = " ") %>%
    as.data.frame()

  if (save_clean){
    usethis::use_data(slopewater, overwrite = T)
  } else {
    return(slopewater)
  }

}
get_slopewater(save_clean = T)
