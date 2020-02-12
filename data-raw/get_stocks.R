# Processing single-species stock mortality and biomass data

# Read more about this data at https://noaa-edab.github.io/tech-doc/stockstatus.html

library(dplyr)
library(tidyr)
library(ggplot2)

data.dir <- here::here('data-raw')

get_stocks <- function(save_clean = F){
  assess <- read.csv(file.path(data.dir, "2019assess.csv"))
  decode <- read.csv(file.path(data.dir, "2019decoder.csv"))

  stock_status <-
    assess %>%
    group_by(Entity.Name) %>%
    filter(Assessment.Year == max(Assessment.Year)) %>%
  #Find last year assessment occurred for each stock
    ungroup() %>%
    left_join(.,decode, by = "Entity.Name") %>% #Join in list of managed species
    dplyr::select(Entity.Name, Assessment.Year, F.Fmsy, B.Bmsy, Council, Code) %>%
  #select column variables to keep
    mutate(id = 1:length(Entity.Name)) %>%
    gather(.,Var, Value,-id,-Entity.Name,-Assessment.Year,-Council,-Code) %>%
  #wide to long
    dplyr::select(-id) %>%
    dplyr::rename(`Last assessment` = Assessment.Year,
                  Stock = Entity.Name) %>% #rename variables for clarity
    mutate(Units = "unitless") %>%
    mutate(Value = replace(Value, which(Code == "N Windowpane" & Var == "F.Fmsy"), NA))

  if (save_clean){
    usethis::use_data(stock_status, overwrite = T)
  } else {
    return(stock_status)
  }
}
get_stocks(save_clean = T)
