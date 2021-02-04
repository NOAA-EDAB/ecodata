
indicators <- ecodata::bennet %>% 
  dplyr::filter(EPU == epu_abbr) 

indicators$Var<- gsub( "Predator", "", indicators$Var)
indicators$Var<- gsub( "Value", "Volume", indicators$Var)
indicators<- indicators %>% 
  separate(Var, c("Guild", "Var") ) %>% 
  dplyr::filter(!Var == "Revenue",
                !Guild == "Total", 
         !Time < 1985) %>% 
  dplyr::group_by(Time, Guild) %>% 
  dplyr::mutate(New = sum(Value)) %>% 
  ungroup()

#dplyr::select(ind2, !c(Units, EPU, Guild, Source, Var, New))

ind2<- indicators %>% 
  dplyr::mutate(Name = paste(Guild, Var)) %>% 
  dplyr::select(-c(Guild, Units, EPU, Source, Var, New)) %>% 
  tidyr::pivot_wider(names_from = Name, values_from = Value) %>% 
  dplyr::select(Time, `Apex Volume`, `Apex Price`, 
                `Benthivore Volume`, `Benthivore Price`, 
                `Benthos Volume`, `Benthos Price`,
                `Planktivore Volume`, `Planktivore Price`,
                `Piscivore Volume`, `Piscivore Price`,
                `Other Volume`, `Other Price` )

Table<- kable(ind2, col.names = c("Time", "Volume", "Price",  "Volume", "Price",
                           "Volume", "Price",  "Volume", "Price", 
                           "Volume", "Price",  "Volume", "Price")) %>% 
  kableExtra::add_header_above(c(" ", "Apex Pred" = 2, "Benthivore" = 2, 
                                 "Benthos" = 2, "Planktivore" = 2, 
                                 "Piscivore" = 2, "Other" = 2))
