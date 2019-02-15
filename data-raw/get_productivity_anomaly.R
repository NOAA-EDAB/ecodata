#processing small fish per large fish biomass indicator

library(dplyr)
library(tidyr)
library(ggplot2)

raw.dir <- here::here("inst","extdata")

load(file.path(raw.dir,"dat_spec_rec_forSOE.Rdata"))
load(file.path(raw.dir,"dat_spec_rec_epu_forSOE.Rdata"))

#Select and rename
epu_rec_anom <- dat_spec_rec_epu_forSOE %>% 
  dplyr::select(Time, EPU = Region, Value, Units, -Source,Var)

#Select, rename, and bind
productivity_anomaly <- dat_spec_rec_forSOE %>% 
  dplyr::select(-Source) %>% 
  mutate(EPU = "All",
         Var = paste("NE LME",Var)) %>% 
  rbind(.,epu_rec_anom) %>% 
  as.data.frame()

usethis::use_data(productivity_anomaly, overwrite = TRUE)

