#' A ggplot2 theme for SOE map figures
#'
#' @export
#'
#' @examples
#'
#' library(rnaturalearth); library(ggplot2)
#' library(dplyr); library(sf)
#'
#' #Map line parameters
#' map.lwd <- 0.4
#'
#' # Set lat/lon window for maps
#' xmin = -76
#' xmax = -66
#' ymin = 36
#' ymax = 45
#' xlims <- c(xmin, xmax)
#' ylims <- c(ymin, ymax)
#'
#' #Get base map and set CRS
#' crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0
#' +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' coast <- ne_countries(scale = 10,
#'                       continent = "North America",
#'                       returnclass = "sf") %>%
#'   sf::st_transform(crs = crs)
#'
#' #Plot with SOE theme
#' ggplot()+
#'   geom_sf(data = coast) +
#'   coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
#'   theme_map()

theme_map <- function(...) {
  ggplot2::theme(
    # axis.line = element_blank(),
    # axis.ticks = element_blank(),
    panel.grid.major = ggplot2::element_line(colour = "white"),
    panel.grid.minor = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.background = ggplot2::element_rect(fill = "white", color = NA)
  )
}
