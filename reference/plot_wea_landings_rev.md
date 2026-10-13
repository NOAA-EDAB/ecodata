# plot revenue from top 10 landings

plot wea_landings_rev.

## Usage

``` r
plot_wea_landings_rev(shadedRegion = NULL, report = "MidAtlantic", n = 0)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- n:

  numeric scalar. The number of species to show (default = n = 0, all
  species)

## Value

flextable object
