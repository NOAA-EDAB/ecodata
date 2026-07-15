# plot community climate vulnerability

Plots time series of proportion of communities in each climate
sensitivity category, or regional sensitivities

## Usage

``` r
plot_community_risks(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "vulnSum",
  plottype = "propcomm",
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

  Character string. Which variable: ocean acidification sensitivity,
  temperature sensitivity, stock biomass sensitivity, total sensitivity,
  total vulnerability ("oaSum","tempSum","stckSum","sensSum","vulnSum")

- plottype:

  Character string. Which plot: proportion of communities (default) or
  regional sensitivity by revenue or landings ("propcomm", "regionrev",
  "regionland")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
