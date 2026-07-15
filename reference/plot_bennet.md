# plot Bennett indicator

Stacked bar charts of price and volume components of revenue by feeding
guild.

## Usage

``` r
plot_bennet(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "guild",
  EPU = "MAB"
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- varName:

  Character string. Which plot ("guild", "total",""total_guild")

- EPU:

  Character string. Which EPU for New England report ("GB", "GOM") Mid
  will always be MAB

## Value

ggplot object
