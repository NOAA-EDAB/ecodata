# plot stock_Status

Kobe plots of regional stock status

## Usage

``` r
plot_stock_status(shadedRegion = NULL, report = "MidAtlantic")
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

## Value

list of 2 items

- p:

  ggplot object

- unknown:

  data frame listing stocks with unklnown status
