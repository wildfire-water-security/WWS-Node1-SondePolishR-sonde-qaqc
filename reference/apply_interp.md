# Map interpolated data back to dataset

Uses interpolated values to attempt to fill in missing data being aware
of maximum gap lengths to fill and duplicates.

## Usage

``` r
apply_interp(data_fill, data_interp, y_var, max_length)
```

## Arguments

- data_fill:

  `data.frame` based on `proj$data` with missing `datetime` values added
  from
  [`prep_interp()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/prep_interp.md).

- data_interp:

  `data.frame` based on `proj$data` with duplicates condensed to a
  single value and missing values interpolated from
  [`run_interp()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/run_interp.md).

- y_var:

  Variable being interpolated.

- max_length:

  The maximum length in hours to fill via interpolation.

## Value

`data_fill` with missing values interpolated.

## Examples

``` r
interp_dfs <- prep_interp(example_sondeproj)
filled_yvar <- run_interp(interp_dfs$interp, "fDOM_QSU", "linear")
data_filled <- apply_interp(interp_dfs$fill, filled_yvar, "fDOM_QSU", 8)
```
