# plot heatwave_peak_date

Map of SST anomaly on date of peak extreme temperature event for a given
year. NOTE DATE NOT CONTAINED IN DATASET. SHOULD BE

## Usage

``` r
plot_heatwave_peak_date(shadedRegion = NULL, report = "MidAtlantic")
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

## Value

ggplot object
