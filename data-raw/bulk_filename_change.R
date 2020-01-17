## Renames Kevins files fron 2017 format to 2016 format
# People need to keep file naming convention and content consistent from year to year

## source this file then run the bulk_filename_change function

bulk_filename_change <- function() {
  
  for (aseason in c("spring","fall")) {
    path <- here::here("data",aseason)
    filenames <- list.files(path)
    for (ifile in 1:length(filenames)) {
      # current file name
      currentFN <- filenames[ifile]
      # create new file name from current
      newFN <- change_file_name(aseason,currentFN)
      # rename file
      print(newFN)
      #file.rename(paste0(path,"/",currentFN),paste0(path,"/",newFN))
    }
  }
  
}


change_file_name <- function(aseason,filename) {
  # split old file names
  spec_yr <- stringr::str_split(filename,"\\.")
  fileParts <- stringr::str_split(spec_yr[[1]][1],"_")
  monthh <- spec_yr[[1]][2]
  dayy <- spec_yr[[1]][3]             
  species <- stringr::str_split(fileParts[[1]][2],"Z")
  # create new file name to match previous years naming convention
  if (species[[1]][1] == "ctyp") speciesN <- "centropages"
  if (species[[1]][1] == "pseudo") speciesN <- "pseudocalanus"
  if (species[[1]][1] == "tlong") speciesN <- "temora"
  
  newFileName <- paste0(speciesN,"_",aseason,"_",fileParts[[1]][3],"_",monthh,"_",dayy, ".RData")
  return(newFileName)
}