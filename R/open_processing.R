#'Open the Rmarkdown document used for processing State of the Ecosystem indicator data.
#'
#'Use this function to access the processing file for turning raw data into derived SOE indicators.
#'
#'
#'@export
#'
#'@examples
#'
#'open_processing()
#'

open_processing <- function(region){
  file.edit(system.file("Rmd","process_raw.Rmd",package = "soe"))
}

