# Processing for Chlorophyll a and Primary Production data

# More information about these data are available at https://noaa-edab.github.io/tech-memo/chl-pp.html

library(stringr)
library(dplyr)
library(tidyr)

raw.dir <- here::here('inst',"extdata")


get_chl_pp <- function(save_clean = F){
  
  # Find relevant files and load them into workspace
  files = list.files(raw.dir, pattern="OCCCI|MODIS")
  for (i in 1:length(files)) assign(files[i], read.csv(file.path(raw.dir,files[i])))
  
  chl_pp <- NULL
  for (i in ls()){
    if (str_detect(i, "OCCCI|MODIS")){
      assign('chl_pp', rbind(chl_pp, get(i)))    
    }
  }
  
  chl_pp <- chl_pp %>%
    mutate(ALGORITHM = word(str_replace(ALGORITHM, "_", " "))) %>% 
    unite(.,VARIABLE, c("VARIABLE","SENSOR","ALGORITHM"), sep = " ") %>% 
    dplyr::select(TIME, UNITS, VARIABLE, VALUE, REGION) %>% 
    dplyr::rename(Time = TIME, Units = UNITS, Var = VARIABLE,
                  EPU = REGION, Value = VALUE)
  
  if (save_clean){
    usethis::use_data(chl_pp, overwrite = T)
  } else {
    return(chl_pp)
  }
  
}
