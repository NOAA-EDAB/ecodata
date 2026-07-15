# NEFSC survey data broken into season and aggregate group biomass

These data contain times series of CPUE, kg tow ^-1, of aggregated
species groups from NEFSC surveys.

## Usage

``` r
aggregate_biomass
```

## Format

A data set containing 52286 rows and 5 columns.

- Time: Survey year.

- Value: Value of variable `Var`

- Var: Includes SOE species grouping, season that survey occurred (fall
  or spring), and variable calculated. The specific variables included
  are stratified mean biomass with associated confidence intervals,
  coefficients of variation, and standard errors.

- EPU: Ecological Production Unit (EPU) where data originated. Here
  "MAB" refers to the Mid-Atlantic Bight.

- Units: Units of variable `Var`.

## Details

More information available at
<https://noaa-edab.github.io/tech-doc/survdat.html>
