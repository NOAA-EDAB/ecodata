# plot comdat

Plot time series of commercial landings or revenue for various
aggregations.

## Usage

``` r
plot_comdat(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "landings",
  plottype = "total",
  NAFOyear = 2019,
  n = 0
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- varName:

  Character string. Which Variable to plot ("landings", "revenue")

- plottype:

  Character string. Which plot ("total", "guild")

- NAFOyear:

  Numeric value. Year that NAFO landings are separated. Defaults to
  2019, as NAFO data was missing 2019-2021 in 2022.

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
