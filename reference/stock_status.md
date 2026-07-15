# Managed species stock status

Stock status for MAFMC and NEFMC managed species in New England and the
Mid-Atlantic.

## Usage

``` r
stock_status
```

## Format

104 rows and 7 columns

- Stock: Stock identifier.

- Last assessment: Last year that a stock assessment was performed for a
  given stock.

- Council: Management body (MAFMC, NEFMC, or Both).

- Code: Shortened identifier for stocks.

- Var: Either `F.Fmsy`, representing the ratio of fishery mortality to
  fishery mortality at maximum sustainable yield (or proxy), or
  `B.Bmsy`, defined as the ratio of estimated stock biomass to estimated
  biomass at maximum sustainable yield (or proxy).

- Value: Value of variable `Var`.

- Units: Units of variable `Var`.

## Details

More information about this data set may be found at
<https://noaa-edab.github.io/tech-doc/stockstatus.html>.
