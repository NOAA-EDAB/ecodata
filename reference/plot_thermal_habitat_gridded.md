# plot thermal habitat persistence

plots thermal_habitat_gridded data set. Current SOE Year only

## Usage

``` r
plot_thermal_habitat_gridded(
  shadedRegion = NULL,
  report = "MidAtlantic",
  year = NULL,
  thresholds = c(15, 24),
  depths = "AllDepths"
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report (Shelfwide indicator, use
  "MidAtlantic" only)

- thresholds:

  numeric vector for the temperature thresholds (0.5 degree increments
  from 0.5 to 30)

- depths:

  Character string. Which depth bands do you want to plot
  ('0-25m','25-100m','100-300m','AllDepths')

## Value

ggplot object
