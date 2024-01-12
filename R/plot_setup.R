#' Setup all plot defaults
#'
#' Single function setting up plot defaults for SOE, catalog, presentations, and other products
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10), passed from plot function
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland"), passed from plot function
#'
#' @return list of settings
#'
#'
#' @export
#'
plot_setup <- function(shadedRegion = shadedRegion,
                       report = report){

  #library(sf) # now needed in function for filter to work on sf object? https://github.com/r-spatial/sf/issues/1381
  # create a 10 year window based on current month
  if(is.null(shadedRegion)) {
    currentMonth <- lubridate::month(Sys.Date())
    currentYear <- lubridate::year(Sys.Date())
    if (currentMonth > 4) {
      endShade <- currentYear
    } else {
      endShade <- currentYear - 1
    }
    shadedRegion <- c(endShade-9,endShade)
  }


  plotsettings <- list()

    # from GIS-setup files (identical between reports)
    #CRS
    crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

    #EPU shapefile
    epu_sf <- ecodata::epu_sf |>
      sf::st_as_sf() |>
      dplyr::filter(EPU %in% c("MAB","GB","GOM"))

    #Map line parameters
    map.lwd <- 0.4

    # Set lat/lon window for maps
    xmin = -77
    xmax = -65
    ymin = 36
    ymax = 45

    ################################################################
    ### Different lat and lons used for bt seasonal anomalies ####
    ### Need to think about that ##############################
    ################################################################

    #Time series constants
    shade.alpha <- 0.3
    shade.fill <- "lightgrey"
    lwd <- 1
    pcex <- 2
    trend.alpha <- 0.5
    trend.size <- 2
    hline.size <- 1
    line.size <- 2
    hline.alpha <- 0.35
    hline.lty <- "dashed"
    label.size <- 5
    hjust.label <- 1.5
    letter_size <- 4
    errorbar.width <- 0.25
    feeding.guilds1<- c("Piscivore","Planktivore","Benthivore","Benthos")
    feeding.guilds <- c("Apex Predator","Piscivore","Planktivore","Benthivore","Benthos")
    x.shade.min <- shadedRegion[1]
    x.shade.max <- shadedRegion[2]

    ## Additional parameters for 4 panel maps eg. plot_seasonal_sst_anomaly_gridded
    ###################################################
    ###################################################

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
      #epu <- "New England" # human dimensions
      epu <- "Gulf of Maine and Georges Bank" #macrofauna
      #epu <- "Geroges Bank" #LTL
      #epu_abbr <- "GB" #LTL

      epu_abbr <- c("GOM","GB")
      region <- "New England"
      region_abbr <- "NE"
    }

    #GIS directory
    #gis.dir <- here::here("data-raw","gridded")

    plotsettings <- list(crs = crs,
                         epu_sf = epu_sf,
                         map.lwd = map.lwd,
                         xmin = xmin,
                         xmax = xmax,
                         ymin = ymin,
                         ymax = ymax,
                         xlims = c(xmin, xmax),
                         ylims = c(ymin, ymax),
                         shade.alpha = shade.alpha,
                         shade.fill =shade.fill,
                         lwd = lwd,
                         pcex = pcex,
                         trend.alpha = trend.alpha,
                         trend.size = trend.size,
                         line.size = line.size,
                         hline.size = hline.size,
                         hline.alpha = hline.alpha,
                         hline.lty = hline.lty,
                         errorbar.width = errorbar.width,
                         label.size = label.size,
                         hjust.label = hjust.label,
                         letter_size = letter_size,
                         feeding.guilds1 = feeding.guilds1,
                         feeding.guilds = feeding.guilds,
                         x.shade.min = x.shade.min,
                         x.shade.max = x.shade.max,
                         series.col = series.col,
                         label = label,
                         council = council,
                         council_abbr = council_abbr,
                         epu = epu,
                         epu_abbr = epu_abbr,
                         region = region,
                         region_abbr = region_abbr,
                        # gis.dir = gis.dir,
                         shadedRegion = shadedRegion
                         )

  return(plotsettings)

}
