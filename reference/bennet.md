# Bennet indicator

Data contain volume and price indicators for commercial fisheries on the
Northeast Continental Shelf, along with their sum totals for each year
and species groupings (i.e. the Bennet indicator). This metric allows
for examining the drivers of revenue change by Ecological Production
Unit (EPU).

## Usage

``` r
bennet
```

## Format

A data set containing 2600 rows and 5 columns.

- Var: Specifies indicator type (price, volume, or total) and associated
  species grouping.

- Value: Value of variable `Var`.

- Time: Year.

- Units: Units of variable `Var`.

- EPU: Ecological Production Unit (EPU) from which data were drawn.

## Details

These data are documented in full at
<https://noaa-edab.github.io/tech-doc/bennet-indicator.html>.
