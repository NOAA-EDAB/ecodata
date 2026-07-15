# plot seasonal_oisst_anom_gridded

plot seasonal_oisst_anom_gridded

## Usage

``` r
plot_seasonal_oisst_anom_gridded(
  shadedRegion = NULL,
  report = "MidAtlantic",
  scale = "celsius"
)
```

## Arguments

- scale:

  character string. celsius or fahrenheit. Default = "celsius"

- season:

  Character string. Season to plot. (Default = NULL, plot all seasons)

- region:

  Character vector. Regional EPUs ("GB","MAB") to overly on figure.
  (Default = NULL, use all)

## Value

ggplot object
