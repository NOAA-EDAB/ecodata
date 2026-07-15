# plot shelf break advection index

plots integrated mass transport through transects along the shelf break

## Usage

``` r
plot_advection(shadedRegion = NULL, report = "MidAtlantic", varName = 3, n = 0)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- varName:

  Numeric scalar. Which variable (months) to plot (3 = 'March', 4 =
  'April', 5 = 'May', 6 = 'June'). Can be a vector

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
