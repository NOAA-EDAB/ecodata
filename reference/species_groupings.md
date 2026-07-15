# Species groupings

Species groupings from a variety of NEFSC EDAB products are included in
this data set.

## Usage

``` r
species_groupings
```

## Format

1022 rows and 15 columns

- COMNAME: Common name of listed species.

- SVSPP: NEFSC BTS database species codes.

- ITISSPP: Integrated Taxonomic Information System (ITIS) species
  identifier.

- NESPP3: Species codes used in CFDBS.

- SCINAME: Scientific name of listed species.

- Fed.managed: Management body. One of `NEFMC`, `MAFMC`, `JOINT`
  (MAFMC/NEFMC), or `NA`.

- SOE.17: Feeding guild groupings used in 2017-2018 SOE reports.

- EMAX: Groupings adapted from Link et al. 2006.

- RPATH: Groupings from Rpath - the R version of the Ecopath with Ecosim
  model.

- SOE.18: Feeding guild groupings used in 2018-2019 SOE reports.

- SizeCat: Size category.

- Min.size: Minimum size.

- Garrison.Link: Groupings adapted from Garrison and Link 2000.

- NEIEA: Simplified feeding guilds for NE-IEA website.

- SOE.20: Feeding guild groupings used in 2020+ SOE reports.

## Details

More information about these species groupings may be found at
<https://noaa-edab.github.io/tech-doc/aggroups.html>.

## References

Garrison, Lance P, and Jason S Link. 2000. “Dietary guild structure of
the fish community in the Northeast United States continental shelf
ecosystem.” *Marine Ecology Progress Series* 202:231–40.

Link, Jason S, Carolyn A Griswold, Elizabeth T Methratta, and Jessie
Gunnard. 2006. Documentation for the energy modeling and analysis
exercise (EMAX). US Department of Commerce, National Oceanic and
Atmospheric Administration.

Lucey, S. 2019. Rpath - the R version of the Ecopath with Ecosim model.
US Department of Commerce, National Oceanic and Atmospheric
Administration. <https://github.com/slucey/RpathDev>.
