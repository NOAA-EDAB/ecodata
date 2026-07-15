# plot heatwave_year

Plot time series of daily detrended surface or bottom temperature for
current year with climatology in the background.

## Usage

``` r
plot_heatwave_year(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "Surface"
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- varName:

  Character string. Which Variable to plot ("Surface", "Bottom") default
  variables Surface and Bottom are detrended

## Value

ggplot object
