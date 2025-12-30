## Benthos Index
raw.dir <- here::here("data-raw/")

# Define file paths for megabenthos index and center of gravity
falmega <- "fallmegabenthosindex.rds"
sprmega <- "springmegabenthosindex.rds"
falmegacog <- "fallmegabenthoscog.rds"
sprmegacog <- "springmegabenthoscog.rds"

# Define file paths for macrobenthos index and center of gravity
falmacro <- "fallmacrobenthosindex.rds"
sprmacro <- "springmacrobenthosindex.rds"
falmacrocog <- "fallmacrobenthoscog.rds"
sprmacrocog <- "springmacrobenthoscog.rds"

get_benthos_index <- function(save_clean = F){

  # Load input files for megabenthos index and center of gravity
  fallmega <- readRDS(file.path(raw.dir, falmega))
  springmega <- readRDS(file.path(raw.dir, sprmega))
  fallmegacog <- readRDS(file.path(raw.dir, falmegacog))
  springmegacog <- readRDS(file.path(raw.dir, sprmegacog))

  # Load input files for macrobenthos index and center of gravity
  fallmacro <- readRDS(file.path(raw.dir, falmacro))
  springmacro <- readRDS(file.path(raw.dir, sprmacro))
  fallmacrocog <- readRDS(file.path(raw.dir, falmacrocog))
  springmacrocog <- readRDS(file.path(raw.dir, sprmacrocog))

  benthos_index<- rbind(fallmega, springmega, fallmegacog, springmegacog, fallmacro, springmacro, fallmacrocog, springmacrocog)

  if (save_clean){
    usethis::use_data(benthos_index, overwrite = T)
  } else {
    return(benthos_index)
  }
}
get_benthos_index(save_clean = T)





