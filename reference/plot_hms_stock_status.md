# plot hms_stock_status

Kobe plot of F/Fmsy vs B/Bmsy for Highly Migratory Species (HMS) stocks.

## Usage

``` r
plot_hms_stock_status(shadedRegion = NULL, report = "MidAtlantic")
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic" default, same for
  both)

## Value

list of 2 items

- p:

  ggplot object

- unknown:

  data frame listing stocks with unklnown status
