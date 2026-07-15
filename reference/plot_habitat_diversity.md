# plot habitat diversity

Plots ecodata::habitat_diversity (Shannon index and species richness)

## Usage

``` r
plot_habitat_diversity(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "Diversity"
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- varName:

  Character string. Which Variable to plot ("Diversity","Richness")

## Value

ggplot object
