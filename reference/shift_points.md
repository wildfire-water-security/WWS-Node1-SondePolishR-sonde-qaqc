# Correct points via an absolute shift

Sometimes a continuous sonde signal will be shifted up or down by a
consistent value for a period of time, this will adjust the dataset for
those values by that set amount.

## Usage

``` r
shift_points(data, par, index, shift_val = NULL)
```

## Arguments

- data:

  a data.frame with sonde data

- par:

  the parameter being corrected

- index:

  the index values of the rows that need to be shifted

- shift_val:

  a list of the slope and int (intercept) to use to shift the data by,
  if `NULL`, it will be guessed using
  [guess_shift](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/guess_shift.md)

## Value

a data.frame with the values adjusted

## Examples

``` r
example_sondeproj$data$ODO_mg_L[5:7]
#> [1] 7.072 7.056 7.040
data <- shift_points(example_sondeproj$data, "ODO_mg_L", 5:7)
data$ODO_mg_L[5:7]
#> [1] 8.84 8.82 8.80
```
