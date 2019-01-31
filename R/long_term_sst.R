#' Long-term SST
#' 
#' Long-term sea-surface temperatures were derived from the NOAA extended reconstructed 
#' sea surface temperature data set (ERSST V5). The ERSST V5 dataset is parsed
#' into 2° x 2° gridded bins between 1854-present with monthly temporal resolution.
#' 
#' @format Data contain 165 rows and 5 columns.
#' \itemize{
#'    \item Var: Specifies variable of interest.
#'    \item Value: Value of variable \code{Var}. 
#'    \item Units: Units of variable \code{Var}.
#'    \item EPU: Ecological Production Unit (EPU) from which data were collected or aggregated. An EPU of \code{All} means that the scope of the data set is the entire Northeast US Continental Shelf.
#'    \item Time: Year.
#'  }
#'  
#' @details Data were interpolated in regions with limited spatial coverage, and heavily
#' damped during the period between 1854-1880 when collection was inconsistent
#' (Huang et al. 2017). For this analysis, 19 bins were selected that encompassed 
#' the Northeast US Continental Shelf region (see Friedland and Hare 2007).
#'  
#' @references 
#' Huang, Boyin, Peter W. Thorne, Viva F. Banzon, Tim Boyer, Gennady Chepurin, Jay H. Lawrimore, Matthew J. Menne, Thomas M. Smith, Russell S. Vose, and Huai Min Zhang. 2017. “Extended reconstructed Sea surface temperature, Version 5 (ERSSTv5): Upgrades, validations, and intercomparisons.” Journal of Climate 30 (20):8179–8205. https://doi.org/10.1175/JCLI-D-16-0836.1.
#' Friedland, KD, and JA Hare. 2007. “Long-term trends and regime shifts in sea surface temperature on the continental shelf of the northeast United States.” Continental Shelf Research 27:2313–28.
#'  
#' @source \url{https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.ersst.v5.html}
"long_term_sst"