# Zooplankton stratified abundance for Euphausids and Cnidarians

Estimated zooplankton stratifies abundance on the Northeast US
Continental Shelf.

## Usage

``` r
zoo_strat_abun
```

## Format

90 rows and 5 columns.

- Var: species.

- Value: Value of variable `Var`.

- EPU: Ecological Production Units where data originated.

- Time: Year.

- Units: Units of variable `Var`.

## Details

These data were derived from bimonthly Ecosystem Monitoring (EcoMon)
cruises throughout the Northeast US Continental Shelf, and represent a
different estimation method than what is found in the zooplankton
anomaly data set. Specifically, abundance anomalies were interpolated
following an ordinary kriging approach by season across the Northeast
Shelf Ecosystem. Exact methods used to derived these data are available
at <https://noaa-edab.github.io/ECSA/>.
