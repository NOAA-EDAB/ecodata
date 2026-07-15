# plots Gulf of Maine Salmon

plots gom_salmon data set

## Usage

``` r
plot_gom_salmon(shadedRegion = NULL, report = "NewEngland", n = 0)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
