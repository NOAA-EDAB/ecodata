#' Commercial and recreational fishery social-economic indicators
#' 
#' Data include commercial and recreational fishery engagement and
#' reliance rankings for coastal communities in the Northeast US.  
#' 
#' @format 1862 rows and 8 columns.
#' 
#' \itemize{
#'     \item ComEng_NE16_ct: Commercial engagement ranking (0-4) 
#'     \item RecEng_NE16_ct: Recreational engagement ranking (0-4)
#'     \item ComRel_NE16_ct: Commercial reliance ranking (0-4)
#'     \item RecRel_NE16_ct: Recreational reliance ranking (0-4)
#'     \item PRIMARY_LONGITUDE: Decimal longitude.
#'     \item PRIMARY_LATITUDE: Decimal latitude.
#'     \item REGION: Indicator region; either \code{New England} or \code{Mid-Atlantic}.
#'     \item STATEABBR: Two letter state abbreviation.
#' }
#' 
#' @details 
#' NOAA Fisheries’ Community Social Vulnerability Indicators (CSVIs) were developed using
#' secondary data including social, demographic and fisheries variables. The social and
#' demographic data were downloaded from the American Community Survey (ACS) 5-yr
#' estimates Dataset at the U.S. Census American FactFinder site for coastal communities
#' at the Census Designated Place (CDP) level, and in some cases the County Subdivision
#' (MCD) level. Commercial fisheries data were pulled from the Commercial fisheries database (CFDBS).
#' The recreational fishing information is publicly accessible through the Marine Recreational Information
#' Program (MRIP). More information including processing and indicator derivation steps are available at
#' \url{https://noaa-edab.github.io/tech-memo/fishery-reliance-and-social-vulnerability.html}.
"eng_rel"