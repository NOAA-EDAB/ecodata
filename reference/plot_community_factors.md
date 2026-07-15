# table of community factor indicators

returns community indicators with highest engagement or port activity

## Usage

``` r
plot_community_factors(
  report = "MidAtlantic",
  varName = "Social",
  plottype = "Commercial",
  n = 10
)
```

## Arguments

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- varName:

  Character string. Which variable sets to plot
  ("Social","Economic","Gentrification")

- plottype:

  Character string. Which Fishery to plot ("Commercial","Recreational")

- n:

  Numeric scalar. Number of rows to return (top in plottype category
  port activity or recreational engagement)

## Value

dataframe
