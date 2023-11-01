#' Setup all plot defaults
#'
#' Single function setting up plot defaults for SOE, catalog, presentations, and other products
#'
#' @param EPUs Character string. Which SOE report ("GB","MAB")
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot
#' @param shade.fill Character string. Color of shaded region. (Default = "lightgrey)
#' @param shade.alpha Numeric scalar. Alpha of shaded region (Default = 0.5)
#'
#' @return list of settings
#'
#'
#' @export
#'
plot_setup <- function(shadedRegion=c(2014,2023),
                       report = report){

  plotsettings <- list()

  # from GIS-setup files (identical between reports)
  #CRS
  crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

  #EPU shapefile
  epu_sf <- ecodata::epu_sf %>%
    filter(EPU %in% c("MAB","GB","GOM"))

  #Map line parameters
  map.lwd <- 0.4

  # Set lat/lon window for maps
  xmin = -77
  xmax = -65
  ymin = 36
  ymax = 45
  xlims <- c(xmin, xmax)
  ylims <- c(ymin, ymax)

  #Time series constants
  shade.alpha <- 0.3
  shade.fill <- "lightgrey"
  lwd <- 1
  pcex <- 2
  trend.alpha <- 0.5
  trend.size <- 2
  hline.size <- 1
  hline.alpha <- 0.35
  hline.lty <- "dashed"
  label.size <- 5
  hjust.label <- 1.5
  letter_size <- 4
  feeding.guilds1<- c("Piscivore","Planktivore","Benthivore","Benthos")
  feeding.guilds <- c("Apex Predator","Piscivore","Planktivore","Benthivore","Benthos")
  x.shade.min <- shadedRegion[1]
  x.shade.max <- shadedRegion[2]

  #Define constants for figure plot
  series.col <- c("indianred","black")

  #Function for custom ggplot facet labels
  label <- function(variable,value){
    return(facet_names[value])
  }

  # from human dimensions setup files, region specific
  # specify report as argument when calling individual plot functions

  if(report == "MidAtlantic"){
    #Council
    council <- "Mid-Atlantic Fishery Management Council"
    council_abbr <- "MAFMC"

    #Region identifiers
    epu <- "Mid-Atlantic Bight"
    epu_abbr <- "MAB"
    region <- "Mid-Atlantic"
    region_abbr <- "MA" #Some commercial data organized by "MA" or "NE" regions, not by EPU
  }

  if(report == "NewEngland"){
    #Council
    council <- "New England Fishery Management Council"
    council_abbr <- "NEFMC"

    #Region identifiers
    epu <- "New England"
    epu_abbr <- c("GOM","GB")
    region <- "New England"
    region_abbr <- "NE"
  }

  return(plotsettings)

}
