# plot north atlantic right whale abundance

plots narw data set. calf and adult abundance. This is a shelf wide
product

## Usage

``` r
plot_narw(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "adult",
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

  Character string. Which variable to plot ("adult","calf")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
