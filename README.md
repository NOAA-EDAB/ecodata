# ecodata

`ecodata` is an R data package developed by the Ecosystems Dynamics and Assessment Branch of the Northeast Fisheries Science Center
for use in State of the Ecosystem (SOE) reporting. SOE reports are high-level overviews of ecosystem indicator status and trends occurring
on the Northeast Continental Shelf. Unless otherwise stated, data are representative of specific Ecological Production Units (EPUs), referring to 
the Mid-Atlantic Bight (MAB), Georges Bank (GB), Gulf of Maine (GOM), and Scotian Shelf (SS). SOE reports are developed for US Fishery
Management Councils (FMCs), and therefore indicator data for Scotian Shelf are included when available, but this is not always the case. **Please
consult** the [technical documentation](https://noaa-edab.github.io/tech-memo/) of SOE indicators before using these data sets.

## Using this package

1.  Use the command `devtools::install_github("noaa-edab/ecodata")` to install the package.
2.  Load the package into your environment with `library(ecodata)`

## Loading data sets

1.  All derived data sets are available once the package has been loaded into the environment. View available data sets using the syntax `ecodata::...`

<p align="center"> 
<img src="https://media.giphy.com/media/KVFf3gvG6z2JvHqY9y/giphy.gif"/>
</p>

## Using geom_gls()

Also included in this package is a "geom" extension of `ggplot2` for assessing trends in time series. This function fits four trend models to each 
series, uses AICc to select the best model fit, and then implements a likelihood-ratio test to determine if a trend is present. If a significant
trend is present (*P* < 0.05), then the trend line is plotted with the series. By default, a purple line color is assigned to negative trends and orange to positive trends. More detailed information about this method is available
[here](https://noaa-edab.github.io/tech-memo/trend-analysis.html). 

`geom_gls()` follows the same rules as other `ggplot` stats/geoms. For example,

```
m <- 0.1
x <- 1:30
y <-  m*x + rnorm(30, sd = 0.35)

data <- data.frame(x = x,
                  y = y)

#Plot series with trend 
ggplot2::ggplot(data = data,aes(x = x, y = y)) +
  geom_line() +
  geom_gls()
```

produces 

![geom_gls](https://github.com/NOAA-EDAB/ecodata/blob/master/docs/geom_gls.png)

