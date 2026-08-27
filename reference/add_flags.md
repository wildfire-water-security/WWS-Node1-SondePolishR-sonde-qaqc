# Add new flag to dataset

Safely adds a flag to a specific parameter without overwriting old
flags. Also maintains a consistent order so that additional flags can be
identified with version control.

## Usage

``` r
add_flags(data, y_var, index, flag)
```

## Arguments

- data:

  Data.frame with sonde data.

- y_var:

  The column to add flag to.

- index:

  Row numbers to add flag to.

- flag:

  Flag to add to the data.frame.

## Value

a `data.frame` with the same dimensions as `data` with the flags added
to the appropriate column.

## Examples

``` r
data <- add_flags(example_sondeproj$data, "fDOM_QSU", 2:7, "TEST01")
data$fDOM_QSU_flag[1:8]
#> [[1]]
#> [1] "RM01"
#> 
#> [[2]]
#> [1] "RM01"   "TEST01"
#> 
#> [[3]]
#> [1] "RM01"   "TEST01"
#> 
#> [[4]]
#> [1] "RM01"   "TEST01"
#> 
#> [[5]]
#> [1] "TEST01"
#> 
#> [[6]]
#> [1] "TEST01"
#> 
#> [[7]]
#> [1] "TEST01"
#> 
#> [[8]]
#> [1] NA
#> 
```
