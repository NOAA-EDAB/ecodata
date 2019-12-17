library(dplyr)
library(tidyr)

#Get raw
raw.dir <- here::here("data-raw") #input raw

get_commontern_MAB <- function(save_clean = F){

  d <- read.csv(file.path(raw.dir,"commontern_MAB.csv"))

  commontern_MAB <- d %>%
    dplyr::select(-c(COUNTY, LONGITUDE, LATITUDE)) %>%
    rename(Var = "BARRIER.ISLAND",
           "2003" = "X2003.NO.COTE.PRS",
           "2008" = "X2008.NO.COTE.PRS",
           "2013" = "X2013.NO.COTE.PRS",
           "2018" = "X2018.NO.COTE.PRS") %>%
    gather(., key = "Time", value = "Value", -Var) %>%
    mutate(EPU = c("MAB"),
           Units = c("Number of breeding pairs"))

  if (save_clean){
    usethis::use_data(commontern_MAB, overwrite = T)
  } else {
    return(commontern_MAB)
  }

}
get_commontern_MAB(save_clean = T)

