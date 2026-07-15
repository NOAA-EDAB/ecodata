# plot chl_pp

Plot time series of chlorophyll a (chl), primary production (pp),
weekly, monthly, or anomalies.

## Usage

``` r
plot_chl_pp(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "chl",
  plottype = "weekly",
  year = NULL,
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

  Character string. Which Variable to plot ("chl","pp","size")

- plottype:

  Character string. Which plot ("weekly", "monthly") Weekly and monthly
  plots are for both variables.

- year:

  Numeric value. Optional. Year for weekly plot, defaults to max year in
  data.

- EPU:

  Character string. Which EPU for New England report ("GB", "GOM") Mid
  will always be MAB

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
