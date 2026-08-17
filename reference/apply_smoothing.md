# Apply smoothing functions to data

Applies the selected smoothing method to the specified data range.

## Usage

``` r
apply_smoothing(data, y_var, method, index = NULL, k = 7)
```

## Arguments

- data:

  A `data.frame` with the data to smooth (must have the columns Index
  and y_var)

- y_var:

  Character specifying the variable to apply smoothing to.

- method:

  Character specifying the method to use for smoothing. Options include
  "rollmean", "rollmedian", "savgol", and "kalman". See details for
  details on the different methods.

- index:

  Index values of the rows that need to be smoothed

- k:

  A adjustment parameter for the selected method (see details).

## Value

data with the values within `range` replaced with the smoothed values

## Details

The following smoothing methods are currently supported:

- **rollmean**: Based on the `rollmean` function from the `zoo` package.
  Replaces data with a rolling mean value. The `k` parameter is used to
  control the number of points to include in the mean calculation.

- **rollmedian**: Based on the `rollmedian` function from the `zoo`
  package. Replaces data with a rolling median value. The `k` parameter
  is used to control the number of points to include in the median
  calculation, should be odd but will convert to an odd number with a
  warning.

- **savgol**: Based on the `savgol` function from the `pracma` package.
  Applies a Savitzky–Golay filter which fits a fourth order polynomial
  to a sliding range of data. The `k` parameter is used controls the
  number of points in the window.

- **savgol**: Based on the `dlmSmooth` function from the `dlm` package.
  Applies a Kalman filter which process model to the data. The `k`
  parameter is used controls the amount of smoothing.

## Examples

``` r
smoothed <- apply_smoothing(example_data, "Temp_C", "rollmean", k=100)
ggplot2::ggplot(example_data, ggplot2::aes(x=DateTime_rd, y=Temp_C)) +
ggplot2::geom_line(color="black", na.rm=TRUE) +
ggplot2::geom_line(data=smoothed, color="darkred", na.rm=TRUE)
```
