# plot cold_pool_sf

Creates a map of annual cold pool area highlighting max, min, and most
recent year, with inset time series of area.

## Usage

``` r
plot_cold_pool_sf(shadedRegion = NULL, report = "MidAtlantic")
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic" only, default)

## Value

ggplot object
