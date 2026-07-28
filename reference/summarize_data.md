# Summarize data to a different time period

Used to take data and aggregate it to a different interval.

## Usage

``` r
summarize_data(data, frequency, sum_method)
```

## Arguments

- data:

  a `data.frame` to summarize

- frequency:

  a `Period` object created using
  [`lubridate::period()`](https://lubridate.tidyverse.org/reference/period.html)
  specifying the time frame to summarize over

- sum_method:

  the summary method to use to summarize the data

## Value

a \`data.frame.

## Examples

``` r
summarize_data(example_sondeproj$data, lubridate::period(1, "month"), "mean")
#> # A tibble: 6 × 13
#>   DateTime_rd         ODO_mg_L ODO_mg_L_flag SpCond_uS_cm SpCond_uS_cm_flag
#>   <dttm>                 <dbl> <chr>                <dbl> <chr>            
#> 1 2024-07-01 00:00:00     8.33 CH01                  63.4 NA               
#> 2 2024-08-01 00:00:00     8.85 NA                    66.8 NA               
#> 3 2024-09-01 00:00:00     9.43 NA                    68.9 NA               
#> 4 2024-10-01 00:00:00    10.3  NA                    69.6 NA               
#> 5 2024-11-01 00:00:00    10.7  NA                    45.8 NA               
#> 6 2024-12-01 00:00:00    11.4  NA                    43.3 NA               
#> # ℹ 8 more variables: Temp_C <dbl>, Temp_C_flag <chr>, Turbidity_FNU <dbl>,
#> #   Turbidity_FNU_flag <chr>, fDOM_QSU <dbl>, fDOM_QSU_flag <chr>, pH <dbl>,
#> #   pH_flag <chr>
```
