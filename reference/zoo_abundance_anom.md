# Zooplankton abundance anomaly

Estimated zooplankton abundance anomaly on the Northeast US Continental
Shelf.

## Usage

``` r
zoo_abundance_anom
```

## Format

129 rows and 5 columns.

- Var: species.

- Value: Value of variable `Var`.

- Units: Units of variable `Var`.

- EPU: Ecological Production Units where data originated.

- Time: Year.

## Details

These data were derived from bimonthly Ecosystem Monitoring (EcoMon)
cruises throughout the Northeast US Continental Shelf, and represent a
different estimation method than what is found in the zooplankton
anomaly data set. Specifically, abundance anomalies were interpolated
following an ordinary kriging approach by season across the Northeast
Shelf Ecosystem. Exact methods used to derived these data are available
at <https://noaa-edab.github.io/ECSA/>.
