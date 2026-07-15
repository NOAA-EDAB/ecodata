# plot wind revenue

plot wind_revenue data set. Only 5 species are plotted.

## Usage

``` r
plot_wind_revenue(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "landing",
  plottype = "facets",
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

  Character string. Which Variable to plot ("landing","value")

- plottype:

  Character string. Which plot ("facets", "nofacets")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
