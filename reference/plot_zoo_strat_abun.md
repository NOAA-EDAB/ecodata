# plot abundance of Euphasids and Cnids

Plots zoo_abund_strat data set

## Usage

``` r
plot_zoo_strat_abun(
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

  Character string. Which EPU in the report ("GB", "GOM", "MAB")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
