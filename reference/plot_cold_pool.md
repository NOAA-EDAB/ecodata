# plot cold_pool

Create 3 panel plot of cold pool temperature, extent, and persistence
time series.

## Usage

``` r
plot_cold_pool(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = NULL,
  n = 0
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic" only, default)

- varName:

  Character string. Which variable to plot (NULL, "cold_pool",
  "persistence","extent"). NULL plots all three.

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
