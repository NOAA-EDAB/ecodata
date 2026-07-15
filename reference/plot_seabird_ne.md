# plot New England sea bird population

Plots seabird_ne data set. Diet diversity, prey composition,

## Usage

``` r
plot_seabird_ne(
  shadedRegion = NULL,
  report = "NewEngland",
  varName = "diversity",
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

  Character string. Variable to plot ("diversity","productivity","prey")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
