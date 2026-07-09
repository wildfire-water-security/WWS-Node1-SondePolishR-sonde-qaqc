# Apply a drift correction to a parameter

Applies a linear correction to data based on the final "corrected" value
often determined via a freshly calibrated sonde compared to the sonde
that's been deployed ("uncorrected").

## Usage

``` r
apply_drift_shift(x, rows, corrected, uncorrected)
```

## Arguments

- x:

  Vector of data to apply correction to.

- rows:

  Row numbers of the values within x that should be corrected.

- corrected:

  Value of the corrected end value, used to determine how much to shift
  data.

- uncorrected:

  Value of the uncorrected value, used to determine how much to shift
  data.

## Value

\`x\` with the drift shift applied.

## Examples

``` r
rows <- example_data$FileName == "example-data1.csv"
x_shift <- apply_drift_shift(example_data$fDOM_QSU, rows, 17.49, 21.71)
```
