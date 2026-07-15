# plot zooplankton abundance anomaly

Plots zoo_abundance_anom data set

## Usage

``` r
plot_zoo_abundance_anom(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "copepod",
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

  Character string. Which variable to plot ("copepod","euphausid")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
