#Ichthyoplankton species counts, diversity, and abundance

library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

raw.dir <- here::here("data-raw")

ichthyo_spec_counts <- read_excel(file.path(raw.dir,"NEFSCIchthyoplanktonSpeciesCount_v3_3.xlsx")) %>% 
  dplyr::select(-Source) %>% 
  dplyr::rename(Time = Year,
                EPU = Region) %>% 
  mutate(Var = paste(Season,str_replace_all(Var, "_", " ")),
         EPU = ifelse(EPU == "all", "All", 
                      EPU)) %>% 
  dplyr::select(-Season) %>% 
  mutate(Value  = as.numeric(Value))

ichthyo_diversity <- read_excel(file.path(raw.dir,"NEFSCIchthyoplanktonDiversity_v3_3.xlsx")) %>% 
  dplyr::select(-Source) %>% 
  dplyr::rename(Time = Year,
                EPU = Region) %>% 
  mutate(Var = paste(Season,str_replace_all(Var, "_", " ")),
         EPU = ifelse(EPU == "all", "All", 
                      EPU)) %>% 
  dplyr::select(-Season) %>% 
  mutate(Value  = as.numeric(Value)) %>% 
  rbind(.,ichthyo_spec_counts)

usethis::use_data(ichthyo_diversity, overwrite = T)

# ichthyo_abundance <- read_excel(file.path(raw.dir,"NEFSCIchthyoplanktonAbundance_v3_3.xlsx")) %>% 
#   dplyr::select(-Source) %>% 
#   dplyr::rename(Time = Year,
#                 EPU = Region) %>% 
#   mutate(Var = paste(Season,str_replace_all(Var, "_", " ")),
#          EPU = ifelse(EPU == "all", "All", 
#                       EPU)) %>% 
#   tidyr::gather(.,Var,Value,-Time,-Season,-Var,-Units,-EPU) %>% 
#   unite(.,Var, c("Season","Var"), sep = " ") %>% 
#   mutate(Value  = as.numeric(Value))
# 
# usethis::use_data(ichthyo_abundance, overwrite = T)
