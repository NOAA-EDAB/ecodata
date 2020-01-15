library(sf)
library(raster)
library(stars)
library(stringr)
library(dplyr)

##Grab rasters from https://drive.google.com/drive/folders/114nR96d2gx6FDHciNa1dK3WLCrbrwqZH
# grab 1_yr for both spring and fall
# and collapse into rasterstack

#A function to collapse many RasterLayers (saved as .RData files) within a directory to a RasterStack
collapse_into_stack <- function(folder_path){
  p <- folder_path
  message(p)


  #Load each files in the directory and put it into a RasterStack
  out <- raster::stack()
  for (i in 1:length(list.files(p))){

    f <- list.files(p)[i]

    if (stringr::str_detect(f, "\\.RData|\\.rdata|\\.Rdata")){

      fname <- stringr::str_split(f, "\\.RData|\\.rdata|\\.Rdata")[[1]][1]
      obj <- loadRData(file.path(p,f))


      assign("out",stack(out,obj))
      names(out)[i] <- fname
      message(fname)
    } else {
      message(paste0(f, " is not a .RData file"))
    }

  }


  return(out)
}


# A handy function to rename .Rdata files
# (https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file)
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

raw.dir <- "c:/users/kimberly.bastille/desktop/Zoo"

#Zooplankton
for (k in c("Spring","Fall")){
  for (i in c("rasters_1yr")){
    print(paste(k,i))
    # suppressMessages(
    assign(paste0("zoo_",k,"_",i),
           collapse_into_stack(folder_path = file.path(raw.dir, k,i)))
    # )
    save(list = paste0("zoo_",k,"_",i), file = file.path(raw.dir,paste0("zoo_",k,"_",i,".rdata")))
  }
}

save(file = data.dir, "gridded")
