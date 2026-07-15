# plot aggregate_biomass

Plots faceted spring and fall survey biomass time series by aggregate
group

## Usage

``` r
plot_aggregate_biomass(
  shadedRegion = NULL,
  report = "MidAtlantic",
  EPU = "MAB",
  n = 0
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- EPU:

  Character string. Which EPU for New England report ("GB", "GOM") Mid
  will always be MAB

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
