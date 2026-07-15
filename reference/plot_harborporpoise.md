# plot harborporpoise

Time series of annual and 5 year rolling mean harbor porpoise bycatch
with potential biological removal (PRB) threshold. Same plot for both
regions.

## Usage

``` r
plot_harborporpoise(shadedRegion = NULL, report = "MidAtlantic")
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

## Value

ggplot object
