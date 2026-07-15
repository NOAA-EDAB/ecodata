# plot commercial_div

Plot time series of commercial fleet diversity (fleet count, fleet
revenue, or species permit revenue). Mid Atlantic or New England, no
EPUs.

## Usage

``` r
plot_commercial_div(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "Fleet count",
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

  Character string. Which Variable to plot ("Fleet count", "Fleet
  diversity in revenue", "Permit revenue species diversity")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
