#'Open Mid-Atlantic or New England State of the Ecosystem reports
#'
#'Use this function to open SOE Rmarkdown documents into your RStudio environment.
#'
#'@param region Enter "mab" or "ne".
#'
#'@export
#'
#'@examples
#'
#'open_soe(region = "mab")
#'

open_soe <- function(region){
  file.edit(system.file("Rmd",paste0("SOE_",toupper(region),"_2019.Rmd"),package = "soe"))
}

