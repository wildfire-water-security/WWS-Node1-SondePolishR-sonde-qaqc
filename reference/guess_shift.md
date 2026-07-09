# Guess the amount to shift observations

Sometimes a continuous sonde signal will be shifted up or down by a
consistent value for a period of time. This function will use the data
before and after the selected observations to guess the appropriate
value to correct the data by

## Usage

``` r
guess_shift(data, par, index)
```

## Arguments

- data:

  a data.frame with sonde data

- par:

  the parameter being corrected

- index:

  the index values of the rows that need to be shifted

## Value

a numeric with the guessed shift value based on the parameter

## Examples

``` r
guess_shift(example_sondeproj$data, "ODO_mg_L", 5:7)
#> $slope
#> [1] -0.004
#> 
#> $int
#> [1] 1.768
#> 
```
