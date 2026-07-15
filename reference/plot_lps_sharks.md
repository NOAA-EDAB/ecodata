# plot shark fishing landings from large pelagics survey

Plots lps_sharks data set

## Usage

``` r
plot_lps_sharks(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "Total",
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

  Character string. Which variable to plot ("Total", "Blue_Shark",
  "Common_Thresher", "Shortfin_Mako") Default plot is Total pelagic
  sharks.

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
