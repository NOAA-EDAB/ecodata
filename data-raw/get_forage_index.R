## Forage Index
raw.dir<- here::here("data-raw/")
#ann<- "annualforageindex - Sarah Gaichas - NOAA Federal.rds"
fal<- "fallforageindex - Sarah Gaichas - NOAA Federal.rds"
spr<- "springforageindex - Sarah Gaichas - NOAA Federal.rds"
falcog <- "fallforagecog - Sarah Gaichas - NOAA Federal.rds"
sprcog <- "springforagecog - Sarah Gaichas - NOAA Federal.rds"

get_forage_index <- function(save_clean = F){

  #annual<-readRDS(file.path(raw.dir, ann))
  fall<- readRDS(file.path(raw.dir, fal))
  spring<-readRDS(file.path(raw.dir, spr))
  fallcog <- readRDS(file.path(raw.dir, falcog))
  springcog <-readRDS(file.path(raw.dir, sprcog))

  forage_index<- rbind(fall, spring, fallcog, springcog)


  if (save_clean){
    usethis::use_data(forage_index, overwrite = T)
  } else {
    return(forage_index)
  }
}
get_forage_index(save_clean = T)





