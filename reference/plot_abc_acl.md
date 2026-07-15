# plot abc_acl

Plots either stacked bar of quotas/ABCs by fishery ("Stacked" plottype
option) or point comparison of catch with quota/ABC by fishery ("Catch"
plottype option)

## Usage

``` r
plot_abc_acl(shadedRegion = NULL, report = "MidAtlantic", plottype = "Stacked")
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- plottype:

  Character string. Which plot ("Stacked", "Catch")

## Value

ggplot object
