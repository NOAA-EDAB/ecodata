# plot comdat profitability

Plots Geret's profitability indices

## Usage

``` r
plot_comdat_profit(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "all",
  EPU = "MAB",
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

  Character string. Which Variable to plot ("all","profit_index",
  "cost_index", "revenue_index")

- EPU:

  Character string. Which EPU for New England report ("GB", "GOM") Mid
  will always be "MAB"

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
