# Plot ecosystem overfishing indices

Plots ppr dataset. Primary production (PP), Fogarty and Ryther indices.
Primary production required scaled by Primary production and mean
trophic level index

## Usage

``` r
plot_ppr(
  shadedRegion = NULL,
  report = "MidAtlantic",
  EPU = "MAB",
  varName = "ryther",
  threshold = "global"
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- EPU:

  Character string. Which EPU for New England report ("GB", "GOM") Mid
  will always be "MAB"

- varName:

  Character string. Variable to plot
  ("pp","fogarty","ryther","ppr","mtl")

- threshold:

  Character string. Select how thresholds are calculated
  ("global","regional")

## Value

ggplot object
