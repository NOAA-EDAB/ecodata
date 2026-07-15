# plot dissolved oxygen from FishBOT

plots number of days below hypoxia threshold

## Usage

``` r
plot_dissolved_oxygen(shadedRegion = NULL, threshold = 5)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10), passed from plot function

- threshold:

  numeric value. Threshold for hypoxia in mg/L. Default \<= 5 mg/L

## Value

ggplot object
