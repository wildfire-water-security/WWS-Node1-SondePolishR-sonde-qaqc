# Identify missing observations with sonde data

Uses the interval-rounded datetime (`DateTime_rd`) observations to
identify periods of missing observations.

## Usage

``` r
identify_gaps(data, ignore = 60 * 5)
```

## Arguments

- data:

  a `data.frame` with sonde data.

- ignore:

  the length in minutes to ignore gaps

## Value

a `data.frame` with the following columns with a row for each missing
period:

- start: starting datetime of the missing data

- end: ending datetime of the missing data

- gap_length: number of points in the missing section

- user_note: user input information about the duplicated section

## Examples

``` r
identify_gaps(example_data, ignore=0)
#> # A tibble: 8 × 4
#>   start               end                 gap_length user_note
#>   <dttm>              <dttm>                   <dbl> <lgl>    
#> 1 2024-08-20 11:00:00 2024-08-20 11:00:00          1 NA       
#> 2 2024-10-23 14:00:00 2024-10-23 14:15:00          2 NA       
#> 3 2024-12-26 14:45:00 2024-12-26 14:45:00          1 NA       
#> 4 2024-12-27 09:45:00 2024-12-27 09:45:00          1 NA       
#> 5 2024-12-28 15:30:00 2024-12-28 15:30:00          1 NA       
#> 6 2024-12-28 16:00:00 2024-12-28 16:00:00          1 NA       
#> 7 2024-12-29 05:30:00 2024-12-29 06:00:00          3 NA       
#> 8 2024-12-29 22:15:00 2024-12-29 22:15:00          1 NA       
```
