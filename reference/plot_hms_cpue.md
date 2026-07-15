# plot hms_cpue

Plot time series of NEUS Highly Migratory Species (HMS) groups: sharks
or tunas.

## Usage

``` r
plot_hms_cpue(shadedRegion = NULL, report = "MidAtlantic", varName = "shark")
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic" default, same plot
  for both)

- varName:

  Character string. Which Variable to plot ("shark", "tuna"). Sharks are
  categorized as large coastal, pelagic, prohibited, and small coastal,
  while tuna are by species.

## Value

ggplot object
