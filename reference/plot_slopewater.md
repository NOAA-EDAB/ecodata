# plot slopewater

plots warm and labridor slopewater entering the shelf. This is a
shelfwide indicator

## Usage

``` r
plot_slopewater(shadedRegion = NULL, report = "NewEngland", n = 0)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. New England only ("NewEngland")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
