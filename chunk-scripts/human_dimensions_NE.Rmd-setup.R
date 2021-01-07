
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
council <- "New England Fishery Management Council"
council_abbr <- "NEFMC"

#Region identifiers
epu <- "New England"
epu_abbr <- c("GOM","GB")
region <- "New England"
region_abbr <- "NE"
