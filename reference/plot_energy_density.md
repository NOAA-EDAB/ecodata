# plot energy_density

Creates multiplanel plot of energy density time series by species.

## Usage

``` r
plot_energy_density(shadedRegion = NULL, report = "MidAtlantic")
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic" only, default)

## Value

ggplot object
