# Using geom_gls

``` r

library(ecodata)
```

## Using geom_gls()

Also included in this package is a “geom” extension of `ggplot2` for
assessing trends in time series. This function fits four trend models to
each series, uses AICc to select the best model fit, and then implements
a likelihood-ratio test to determine if a trend is present. If a
significant trend is present (*P* \< 0.05), then the trend line is
plotted with the series. By default, a purple line color is assigned to
negative trends and orange to positive trends. More detailed information
about this method is available
[here](https://noaa-edab.github.io/tech-doc/trend-analysis.html).

[`geom_gls()`](https://noaa-edab.github.io/ecodata/reference/geom_gls.md)
follows the same rules as other `ggplot` stats/geoms. For example,

    m <- 0.1
    x <- 1:30
    y <-  m*x + rnorm(30, sd = 0.35)

    data <- data.frame(x = x,
                      y = y)

    #Plot series with trend 
    ggplot2::ggplot(data = data,aes(x = x, y = y)) +
      geom_line() +
      geom_gls()

produces

![](https://raw.githubusercontent.com/NOAA-EDAB/ecodata/master/docs/images/geom_gls.png)
