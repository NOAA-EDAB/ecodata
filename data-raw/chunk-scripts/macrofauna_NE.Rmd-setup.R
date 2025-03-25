
#Image Directory
image.dir <- here::here("docs/images")

#Default Rmd options
knitr::opts_chunk$set(echo = FALSE,
                      message = FALSE,
                      warning = FALSE,
                      fig.align = 'center') #allows for inserting R code into captions

#Plotting and data libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ecodata)
library(here)
library(kableExtra)
library(ggrepel)
library(stringr)
library(patchwork)
library(grid)
library(ggiraph)
library(vegan)
library(rpart)
library(colorRamps)
library(cowplot)

#GIS libraries
library(sf)
library(rgdal)
library(raster)


#GIS directory
gis.dir <- here::here("data-raw","gridded")

#General inline text input for report
#Council
council <- "New England Fishery Management Council"
council_abbr <- "NEFMC"

#Region identifiers
epu <- "Gulf of Maine and Georges Bank"
epu_abbr <- c("GB","GOM")
region <- "New England"
region_abbr <- "NE" #Some commercial data organized by "MA" or "NE" regions, not by EPU 
