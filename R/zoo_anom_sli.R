#' Zooplankton abundance anomalies & small-large index (EcoMon)
#' 
#' Zooplankton abundance anomalies and small-large index derived from EcoMon survey data.
#' 
#' @format 656 rows and 5 columns
#' \itemize{
#'     \item Var: Specifies variable name.
#'     \item Value: Specifies value of variable \code{Var}.
#'     \item EPU: Ecological Production Unit where sampling occurred.
#'     \item Time: Year.
#'     \item Units: Units of variable \code{Var}.
#' }
#' 
#' @details 
#' Abundance anomalies are computed from the expected abundance on the day of sample collection.
#' Abundance anomaly time series are constructed for \emph{Centropages typicus}, \emph{Pseudocalanus} spp.,
#' \emph{Calanus finmarchicus}, and total zooplankton biovolume. The small-large copepod size index 
#' is computed by averaging the individual abundance anomalies of \emph{Pseudocalanus} spp., \emph{Centropages
#' hamatus}, \emph{Centropages typicus}, and \emph{Temora longicornis}, and subtracting the abundance anomaly
#' of \emph{Calanus finmarchicus}. This index tracks the overall dominance of the small bodied copepods
#' relative to the largest copepod in the NEUS region, \emph{Calanus finmarchicus}.
#' 
#' More information about these indicators are available at \url{https://noaa-edab.github.io/tech-doc/zooabund.html}.
#' 
#' @references 
#' Kane, Joseph. 2007. “Zooplankton abundance trends on Georges Bank, 1977-2004.” \emph{ICES Journal of Marine Science} 64 (5):909–19.
#' https://doi.org/10.1093/icesjms/fsm066.
#'
#' Kane, Joseph. 2011. “Multiyear variability of phytoplankton abundance in the Gulf of Maine.” \emph{ICES Journal of Marine Science} 68
#' (9):1833–41. https://doi.org/10.1093/icesjms/fsr122.
#'
#' Morse, R. E., K. D. Friedland, D. Tommasi, C. Stock, and J. Nye. 2017. “Distinct zooplankton regime shift patterns across
#' ecoregions of the U.S. Northeast continental shelf Large Marine Ecosystem.” \emph{Journal of Marine Systems} 165:77–91.
#' https://doi.org/10.1016/j.jmarsys.2016.09.011.
#'
#' Perretti, Charles T., Michael J. Fogarty, Kevin D. Friedland, Jon A. Hare, Sean M. Lucey, Richard S. McBride, Timothy J.
#' Miller, et al. 2017a. “Regime shifts in fish recruitment on the Northeast US Continental Shelf.” \emph{Marine Ecology Progress Series}
#' 574:1–11. https://doi.org/10.3354/meps12183.
"zoo_anom_sli"
