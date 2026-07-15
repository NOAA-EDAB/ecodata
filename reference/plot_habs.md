# plot harmful algal bloom

Plots Alexandrium counts or PSP

## Usage

``` r
plot_habs(shadedRegion = NULL, report = "MidAtlantic", varName = "Alexandrium")
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- varName:

  Character string. Which Variable to plot ("Alexandrium", "PSP")

## Value

ggplot object
