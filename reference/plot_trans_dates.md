# plot SST transition dates and timing

uses ecodata::trans_dates

## Usage

``` r
plot_trans_dates(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "timing",
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

  Character string. Which Variable to plot ("timing","length")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
