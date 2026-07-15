# plot species_dist

Plots species distribution along shelf and depth

## Usage

``` r
plot_species_dist(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "along",
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

  Character string. Which variable to plot ("along","depth")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
