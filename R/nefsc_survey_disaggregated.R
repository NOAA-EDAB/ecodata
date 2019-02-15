#' NEFSC Bottom Trawl Survey (species-level)
#' 
#' The NEFSC Bottom Trawl Survey disaggregated to the level of individual species, with tow proportions
#' of each species within respective feeding guilds. Feeding guilds are simplified grouping structures
#' developed from those proposed by Link et al. (2006) and Garrison and Link (2000). The
#' feeding guild structure includes Apex Predator, Piscivore, Planktivore, Benthivore, Benthos, and Other.
#' Data is presented for both fall and spring bottom trawls surveys.
#' 
#' @format 
#' 
#' \itemize{
#'     \item SVSPP: Species identifier code.
#'     \item Time: Years.
#'     \item EPU: Ecological Production Unit where sampling occurred (MAB = Mid-Atlantic Bight, GB = Georges Bank,
#'     GOM = Gulf of Maine, SS = Scotian Shelf). 
#'     \item Value (kg tow^-1): Annual biomass per tow at the species level.
#'     \item Feeding guild: Feeding guild assigned to each species.
#'     \item Common name: Common name of species of interest.
#'     \item Season: Sampling season (spring or fall).
#'     \item Guild total: Annual feeding guild biomass.
#'     \item Proportion: Proportion of feeding guild biomass by species.
#'     \item Management: Species management body (MAFMC, NEFMC, or JOINT).
#' }
#' 
#' @details More information regarding the NEFSC Bottom Trawl Survey is available at \url{https://noaa-edab.github.io/tech-memo/survdat.html},
#' and further details about feeding guild structure are available at \url{https://noaa-edab.github.io/tech-memo/aggroups.html}.
#' 
#' @references 
#' Garrison, Lance P, and Jason S Link. 2000. “Dietary guild structure of the fish community in the Northeast United States continental shelf ecosystem.”
#'  \emph{Marine Ecology Progress Series} 202:231–40.
#'  
#' Link, Jason S, Carolyn A Griswold, Elizabeth T Methratta, and Jessie Gunnard. 2006. Documentation for the energy modeling and analysis exercise (EMAX).
#'  US Department of Commerce, National Oceanic and Atmospheric Administration.
"nefsc_survey_disaggregated"
