# plot ches_bay_sst

Plot map of sea surface temperature for Chesapeake Bay.

## Usage

``` r
plot_ches_bay_sst(
  shadedRegion = NULL,
  report = "MidAtlantic",
  scale = "celsius"
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic" only, default)

- scale:

  character string. celsius or fahrenheit. Default = "celsius"

## Value

ggplot object
