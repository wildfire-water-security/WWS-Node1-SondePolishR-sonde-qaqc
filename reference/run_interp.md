# Interpolate data gaps

Uses the specified method to fill NA values for a given parameter within
the dataset.

## Usage

``` r
run_interp(data_interp, y_var, method, freq = 1)
```

## Arguments

- data_interp:

  `data.frame` from
  [`prep_interp()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/prep_interp.md)
  with dupes condensed.

- y_var:

  Variable being interpolated.

- method:

  The method to use for interpolation, options include: `linear`,
  `spline`, `random_forest`, and `ts_interp` see details for further
  information about the methods.

- freq:

  The period interval for the time series in days, only used if `method`
  is `ts_interp`.

## Value

a `data_interp` with a extra column added:

- `yvar_fill`: Filled values for `y_var`.

## Details

Currently the follow interpolation options are supported:

- `linear`: Uses the zoo::na.approx() function to linearly fill gaps.

- `spline`: Uses the zoo::na.spline() function to fill gaps via spline
  interpolation (makes smoother curves).

- `random_forest`: Depending on how much data is in the project, this
  option can be a little slow. Uses missForest::missForest() to fill
  gaps. uses responses across all the available parameters to try and
  fill in the missing parameter.

- `ts_interp`: Uses forecast::na.interp() which treats the data as a
  time series and make predictions based on the natural seasonal
  patterns within the data. The seasonality is determined by `freq`. For
  instance, if set to 1, it will account for daily fluctuations. If set
  to 365 it will look at annual fluctuations.

## Examples

``` r
interp_dfs <- prep_interp(example_sondeproj)
filled_yvar <- run_interp(interp_dfs$interp, "fDOM_QSU", "linear")
```
