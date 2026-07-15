# plot recreational data

Plot recdat data set, effort, diversity, and landings

## Usage

``` r
plot_recdat(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "landings",
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

  Character string. Variable to plot
  ("landings","effortdiversity","catchdiversity","effort")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
