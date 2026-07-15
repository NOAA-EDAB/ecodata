# plot WBTS zooplankton indicator

WBTS Zooplankton indicator provided by Runge Lab at the Univeristy of
Maine

## Usage

``` r
plot_wbts_zoo(shadedRegion = NULL, report = "NewEngland", n = 0)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report (only accepts "NewEngland")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
