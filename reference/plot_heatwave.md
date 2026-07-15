# plot heatwave

Time series plots of detrended temperature extreme event maximum
intensity and total days for either Surface or Bottom temperature.

## Usage

``` r
plot_heatwave(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "Surface",
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

  Character string. Which Variable to plot ("Surface", "Bottom")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
