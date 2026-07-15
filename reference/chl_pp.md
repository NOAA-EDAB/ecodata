# Chlorophyll *a* and primary production and phytoplankton size class

These data are time series of remotely sensed chlorophyll *a* (CHL) and
primary production (PP) data from the Northeast Continental Shelf.
Included are median and anomaly CHL and PP estimates for Ecological
Production Units, along with subsets for CHL and PP by phytoplankton
size class. Data from multiple sensors (OC-CCI, SeaWiFS, MODIS-Aqua) and
algorithms (see <https://noaa-edab.github.io/tech-doc/chl-pp.html>) are
presented.

## Usage

``` r
chl_pp
```

## Format

These data contain 19872 rows and 5 columns.

- Var: Specifies temporal sampling resolution
  (`ANNUAL`,`MONTHLY`,`WEEKLY`), variable type (`MEDIAN`, `ANOMALY`,
  `PICO`, `NANO`, `MICRO`), algorithm, and sensor (`OC-CCI`, `SeaWiFS`,
  `MODIS-Aqua`). If `ANOMALY` is specified, `Var` also include a time
  range from which the long-term mean was calculated.

- Value: Value of variable `Var`.

- Units: Units of variable `Var`.

- Time: Time step of `Var`. Time format is given as YYYYMM for monthly
  time steps and YYYYWW for weekly data.

- EPU: Ecological Production Unit (EPU) where sampling occurred.

## Details

Methods used to calculate median and anomaly data are available at
<https://noaa-edab.github.io/tech-doc/chl_pp.html>. Methods used to find
phytoplankton size-class information are forthcoming.
