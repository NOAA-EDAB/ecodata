# Processing species groupings

# These data identify species groupings used in analyses of marine community
# structure and function in the Northeast Large Marine Ecosystem (NE-LME).
# Included are species common names (COMNAME) along with scientific names
# (SCINAME) and several species codes. SVSPP codes are used by the NEFSC
# Ecosystems Surveys Branch in their fishery-independent Survey Database
# (SVDBS) while NESPP3 codes refer to the codes used by the Commercial
# Fisheries Database System (CFDBS) for fishery-dependent data. A third species
# code provided is the ITISSPP, which refers to species identifiers used by the
# Integrated Taxonomic Information System (ITIS). Digits within ITIS codes are
# hierarchical, with different positions in the identifier referring to higher
# or lower taxonomic levels.  More information about the SVDBS, CFDBS, and ITIS
# species codes are available in the links provided below.
#
# The column variable "Fed.managed" refers to the species' management body
# (limited to NEFMC, MAFMC, or jointly managed species ("JOINT")). Group names
# listed under columns containing "SOE" are species groupings that were used in
# the State of the Ecosystem Reports, which are delivered to the New England
# Fishery Management Council (NEFMC) and Mid-Atlantic Fishery Management
# Council (MAFMC) to provide ecosystems context. SOE groupings classify species
# according to feeding guilds derived from Garrison and Link (2000) and Link et
# al. (2006). These groupings are refined annually for more effective reporting
# and differ slightly between years. The columns "EMAX" and "Garrison.Link"
# refer to the species groupings presented in Link et al. (2006) and Garrison
# and Link (2000) respectively.
#
# Species groupings listed in the "NEIEA" column were developed for
# presentation on the Northeast Integrated Ecosystem Assessment (NE-IEA)
# website. These groupings are based on EMAX groupings, but were adjusted based
# on conceptual models developed for the NE-IEA program that highlight focal
# components in the NE-LME (i.e. those components with the largest potential
# for perturbing ecosystem dynamics). NE-IEA groupings were further simplified
# to allow for effective communication through the NE-IEA website.
#
# See the following links for more information regarding the NEFSC ESB Bottom
# Trawl Survey, CFDBS, and ITIS:
# https://www.itis.gov/
# https://inport.nmfs.noaa.gov/inport/item/22561
# https://inport.nmfs.noaa.gov/inport/item/22560
# https://inport.nmfs.noaa.gov/inport/item/27401
#
# More information about the NE-IEA program is available here:
# http://integratedecosystemassessment.noaa.gov

raw.dir <- here::here("data-raw")

get_species_groupings <- function(save_clean = F){

  load(file.path(raw.dir, "SOE_species_list.Rdata"))
  species_groupings <- species %>% tibble::as_tibble()

  if (save_clean){
    usethis::use_data(species_groupings, overwrite = T)
  } else {
    return(species_groupings)
  }
}

get_species_groupings(save_clean = T)

