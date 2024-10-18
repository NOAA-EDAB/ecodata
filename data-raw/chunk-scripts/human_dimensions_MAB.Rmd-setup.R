
# Image Directory
image.dir <- here::here("docs/images")
gis.dir <- here::here("data-raw/gis")
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
#library(patchwork)
library(grid)
library(cowplot)

#GIS libraries
library(sf)
library(rgdal)
library(raster)
library(ggspatial)
library(marmap)


#General inline text input for report

#Council
council <- "Mid-Atlantic Fishery Management Council"
council_abbr <- "MAFMC"

#Region identifiers
epu <- "Mid-Atlantic Bight"
epu_abbr <- "MAB"
region <- "Mid-Atlantic"
region_abbr <- "MA" #Some commercial data organized by "MA" or "NE" regions, not by EPU 
