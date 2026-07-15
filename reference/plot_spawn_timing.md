# plot spawn_timing

Plots time series of maturity stage and associated data for available
stocks

## Usage

``` r
plot_spawn_timing(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "Resting",
  n = 0
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic" only)

- varName:

  Character string. Which variable to plot: maturity stage ("Resting",
  "Ripe", "Spent", "Developing"), number of mature females ("MF"), mean
  sampled bottom temperature ("meanTEMP"), or mean sampled day of year
  ("meanJDAY")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
