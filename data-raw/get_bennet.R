# Process Bennet indicator; price and volume indicators

library(dplyr)
library(tidyr)
library(magrittr)

raw.dir <- here::here('inst','extdata')

get_bennet <- function(save_clean = F){
  
  # Find relevant files and load them into workspace
  files = list.files(raw.dir, pattern="_vi|_pi|_bennet")
  for (i in 1:length(files)) assign(files[i], read.csv(file.path(raw.dir,files[i])))
  
  #Process Bennet indicator data aggregated to the level of EPU (all feeding guilds)
  bennet <- NULL
  for (i in ls()){
    if (stringr::str_detect(i, "_bennet_")){
      epu <- stringr::str_extract(i, "gb|gom|mab") #Extract EPU
      
      #Process into SOE format
      out <- get(i) %>% mutate(EPU = epu,
                               Units = "million USD ($2015)") %>%
        dplyr::select(-X) %>%
        gather(.,Var, Value,-YEAR,-EPU, -Units) %>%
        mutate(Var = paste(Var, "EPU aggregate"),
               EPU = toupper(EPU)) %>%
        dplyr::rename(Time = YEAR) %>% 
        as.data.frame()
      
      assign('bennet',rbind(bennet, out))
      
    }
  }
  
  pi.vi <- NULL
  
  for (i in ls()){
    if (stringr::str_detect(i, "_pi|_vi")){
      epu <- stringr::str_extract(i, "gb|gom|mab")
      indicator <- stringr::str_extract(i, "vi|pi")
      
      out <- get(i) %>%
        mutate(EPU = toupper(epu),
               class = toupper(indicator),
               Units = "million USD ($2015)") %>%
        gather(.,Var,Value,-YEAR, -class, -EPU,-Units) %>% 
        unite(., "Var", c("Var","class"), sep = " ") %>% 
        dplyr::rename(Time = YEAR) %>% 
        as.data.frame()
      
      assign('pi.vi', rbind(pi.vi, out))
      
    }
  }
  
  bennet <- rbind(pi.vi, bennet)
  
  if (save_clean){
    usethis::use_data(bennet, overwrite = T)
  } else {
    return(bennet)
  }
  
}