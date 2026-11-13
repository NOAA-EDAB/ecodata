#' Species groupings
#'
#' Species groupings from a variety of NEFSC EDAB products are included in this data set.
#'
#' @format 1020 rows and 11 columns
#'
#' \itemize{
#'     \item COMNAME: Common name of listed species.
#'     \item SVSPP: NEFSC BTS database species codes.
#'     \item ITISSPP: Integrated Taxonomic Information System (ITIS) species identifier.
#'     \item NESPP3: Species codes used in CFDBS.
#'     \item SCINAME: Scientific name of listed species.
#'     \item Fed.managed: Management body. One of \code{NEFMC}, \code{MAFMC}, \code{JOINT} (MAFMC/NEFMC), or \code{NA}.
#'     \item RPATH: Groupings from Rpath - the R version of the Ecopath with Ecosim model.
#'     \item SizeCat: Size category.
#'     \item SOE.24: Alias column - same contents as the Guild column. Temporary fix to prevent downstream impacts at this point in time.
#'     \item FMP: Fisheries management plan of the listed species, or \code{NA}.
#'     \item Guild: Feeding guild groupings used in the 2027+ SOE reports
#' }
#'
#' @details
#' More information about these species groupings may be found at \url{https://noaa-edab.github.io/tech-doc/aggroups.html}.
#'
#' Guilds were assigned according to the following definitions:
#'
#' \itemize{
#'     \item Benthos:	sessile/benthic-associated organisms that are not predators of managed species
#'     \item Apex predator:	highly migratory species at the highest trophic levels. max size >1.5m
#'     \item Benthivore:	eats benthos; may be an invertebrate
#'     \item Piscivore:	pelagic consumer of fish
#'     \item Planktivore:	pelagic consumer of plankton or early life stage fish
#'     \item Freshwater:	any species that lives in freshwater
#'     \item Flora:	non-animal species (algae, etc)
#'     \item Aquaculture:	species marked with "aquaculture" in their name
#'     \item Other:	species marked "other" or "trash"; squid eggs
#'     }
#'
#' @references
#' Garrison, Lance P, and Jason S Link. 2000. “Dietary guild structure of the fish community in the Northeast United
#' States continental shelf ecosystem.” \emph{Marine Ecology Progress Series} 202:231–40.
#'
#' Link, Jason S, Carolyn A Griswold, Elizabeth T Methratta, and Jessie Gunnard. 2006. Documentation
#' for the energy modeling and analysis exercise (EMAX). US Department of Commerce,
#' National Oceanic and Atmospheric Administration.
#'
#' Lucey, S. 2019. Rpath - the R version of the Ecopath with Ecosim model. US Department of Commerce,
#' National Oceanic and Atmospheric Administration. \url{https://github.com/slucey/RpathDev}.
#'
"species_groupings"
