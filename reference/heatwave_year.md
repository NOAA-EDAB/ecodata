# Marine heatwaves within most recent year

Data include daily heatwave anomaly for most recent year.

## Usage

``` r
heatwave_year
```

## Format

Data set contains 3129 rows and 12 columns

- doy: Day of Year.

- t: Date.

- temp: Temperature.

- seas: Climatological Temperature (1982-max year).

- thresh: Climatological threshold indicator anything above is a
  heatwave.

- threshCriterion: If TRUE then temperature is above the 90th percentile
  of the long term average.

- durationCriterion: If TRUE the temp has been above threshold for 5 or
  more days (surface) and 30 or more days (bottom).

- event: If TRUE indicates heatwave event.

- event_no: Heatwave number.

- EPU: Ecological Production Unit (EPU) where sampling occurred.

- Year: Year

- Var: Variable

## Details

More information including processing and indicator derivation steps are
available at
<https://noaa-edab.github.io/tech-doc/marine_heatwave.html>.
