# plot species groupings by SOE year

plots species_grouping dataset

## Usage

``` r
plot_species_groupings(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "2024"
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- varName:

  Character string. Which year of SOE report ("2024",
  "2020","2018","2017")

## Value

flextable object
