# plot bottom anomaly temperature time series

plots bottom_temp_model_anom data set. Use GLORYS and PSY to supplement

## Usage

``` r
plot_bottom_temp_model_anom(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "seasonal",
  EPU = "MAB",
  plottype = "GLORYS",
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

  Character string. Which variable ("seasonal", "annual")

- EPU:

  Character string. Which EPU for New England report ("GB", "GOM") Mid
  will always be MAB

- plottype:

  character vector. Which source data should be plotted (e.g.
  'GLORYS','MOM6')

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
