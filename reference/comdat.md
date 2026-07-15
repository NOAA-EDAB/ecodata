# Aggregated commercial fisheries data

Aggregated time series of commercial fisheries landings and revenue data
from the Northeast Continental Shelf grouped by feeding guild and
Ecological Production Unit (EPU). Data are presented for NEFMC and MAFMC
managed and unmanaged (i.e. all landings) fisheries, and were derived
from the Commercial Fisheries Database Biological Sample (CFDBS).

## Usage

``` r
comdat
```

## Format

These data include 25143 rows and 5 columns.

- Var: Specifies feeding guild and data type (e.g. managed or unmanaged,
  revenue or landings). Variables including `prop` give the proportion
  of landings or revenue attributed to a specified feeding guild.

- Value: Value of variable `Var`.

- EPU: Ecological Production Unit from which data were aggregated.

- Units: Units of variable `Var`.

- Time: Year.

## Details

These data are fully documented at
<https://noaa-edab.github.io/tech-doc/comdat.html>.
