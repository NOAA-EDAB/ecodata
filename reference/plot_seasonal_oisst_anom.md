# plot SST anomaly (OISST)

plot SST anomaly (OISST)

## Usage

``` r
plot_seasonal_oisst_anom(
  shadedRegion = NULL,
  report = "MidAtlantic",
  EPU = "MAB",
  n = 0
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10), passed from plot function

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland"),
  passed from plot function

- EPU:

  Character string. Which EPU for New England report ("GB", "GOM") Mid
  will always be MAB

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
