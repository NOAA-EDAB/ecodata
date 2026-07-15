# plot exp_n

Plots time series of expected number of species from NEFSC bottom trawl
survey. Plot by season

## Usage

``` r
plot_exp_n(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "fall",
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

  Character string. Which Season to plot ("fall", "spring")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
