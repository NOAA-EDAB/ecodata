### Primary Production Required - Andy Beet and Mike Fogarty

library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

get_ppr <- function(save_clean = F){

  ppr<-readRDS(file.path(raw.dir, "PPR.rds")) %>%
    rename(Time  = YEAR,
           EPU = REGION,
           Value = INDEX) %>%
    mutate(Units = c("%"),
           Var = c("Primary Production Required"))

  if (save_clean){
    usethis::use_data(ppr, overwrite = T)
  } else {
    return(ppr)
  }

}
get_ppr(save_clean = T)
