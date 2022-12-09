# Processing single-species stock mortality and biomass data

# Read more about this data at https://noaa-edab.github.io/tech-doc/stockstatus.html

library(dplyr)
library(tidyr)
library(ggplot2)

data.dir <- here::here('data-raw/')
stock_status_access_csv <- "assess - Sarah Gaichas - NOAA Federal.csv"
stock_status_decoder_csv <-  "decoder - Sarah Gaichas - NOAA Federal.csv"
get_stocks <- function(save_clean = F){
  assess <- read.csv(file.path(data.dir, stock_status_access_csv))
  decode <- read.csv(file.path(data.dir, stock_status_decoder_csv))

  stock_status <-
    assess %>%
    dplyr::group_by(Entity.Name) %>%
    dplyr::filter(Assessment.Year == max(Assessment.Year)) %>%
  #Find last year assessment occurred for each stock
    dplyr::ungroup() %>%
    dplyr::left_join(.,decode, by = "Entity.Name") %>% #Join in list of managed species
    dplyr::select(Entity.Name, Assessment.Year, F.Fmsy, B.Bmsy, Council, Code) %>%
  #select column variables to keep
    dplyr::mutate(id = 1:length(Entity.Name)) %>%
    tidyr::gather(.,Var, Value,-id,-Entity.Name,-Assessment.Year,-Council,-Code) %>%
  #wide to long
    dplyr::select(-id) %>%
    dplyr::rename(`Last assessment` = Assessment.Year,
                  Stock = Entity.Name) %>% #rename variables for clarity
    dplyr::mutate(Units = "unitless") %>%
    dplyr::mutate(Value = replace(Value, which(Code == "N Windowpane" & Var == "F.Fmsy"), NA))

  if (save_clean){
    usethis::use_data(stock_status, overwrite = T)
  } else {
    return(stock_status)
  }
  # metadata ----
  attr(stock_status, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/stockstatus.html"
  attr(stock_status, "data_files")   <- list(
    stock_status_access_csv = stock_status_access_csv,
    stock_status_decoder_csv = stock_status_decoder_csv)
  attr(stock_status, "data_steward") <- c(
    "Sarah Gaichas <sarah.gaichas@noaa.gov>")
}
get_stocks(save_clean = T)
