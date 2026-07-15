# plot bottom anomaly temperature time series using GLORYS

ecodata::bottom_temp_glorys

## Usage

``` r
plot_bottom_temp_glorys(shadedRegion = NULL, report = "MidAtlantic", n = 0)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
