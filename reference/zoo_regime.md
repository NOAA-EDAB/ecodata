# Zooplankton abundance anomaly used in regime shift analysis

Estimated zooplankton abundance anomaly on the Northeast US Continental
Shelf used in the regime shift analysis.

## Usage

``` r
zoo_regime
```

## Format

3458 rows and 4 columns.

- Var: species.

- Value: Value of variable `Var`.

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
